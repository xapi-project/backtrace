(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let my_name = ref (Filename.basename Sys.argv.(0))

let set_my_name x = my_name := x

module Mutex = struct
  include Mutex

  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock ;
    let r = try f () with exn -> Mutex.unlock lock ; raise exn in
    Mutex.unlock lock ; r
end

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i
    :: split_c c (String.sub str (i + 1) (String.length str - i - 1))
  with Not_found -> [str]

type frame = {process: string; filename: string; line: int}

type t = frame list

let empty = []

let frame_of_sexp sexp =
  let open Sexplib0 in
  let open Sexplib0.Sexp_conv in
  match sexp with
  | Sexp.List field_sexps -> (
      let process_field = ref None
      and filename_field = ref None
      and line_field = ref None
      and duplicates = ref []
      and extra = ref [] in
      let rec iter = function
        | Sexp.List (Sexp.Atom field_name :: (([] | [_]) as _field_sexps))
          :: tail ->
            let _field_sexp () =
              match _field_sexps with
              | [x] ->
                  x
              | [] ->
                  Sexp_conv_error.record_only_pairs_expected !my_name sexp
              | _ ->
                  assert false
            in
            ( match field_name with
            | "process" -> (
              match !process_field with
              | None ->
                  let _field_sexp = _field_sexp () in
                  let fvalue = string_of_sexp _field_sexp in
                  process_field := Some fvalue
              | Some _ ->
                  duplicates := field_name :: !duplicates
            )
            | "filename" -> (
              match !filename_field with
              | None ->
                  let _field_sexp = _field_sexp () in
                  let fvalue = string_of_sexp _field_sexp in
                  filename_field := Some fvalue
              | Some _ ->
                  duplicates := field_name :: !duplicates
            )
            | "line" -> (
              match !line_field with
              | None ->
                  let _field_sexp = _field_sexp () in
                  let fvalue = int_of_sexp _field_sexp in
                  line_field := Some fvalue
              | Some _ ->
                  duplicates := field_name :: !duplicates
            )
            | _ ->
                if !Sexp_conv.record_check_extra_fields then
                  extra := field_name :: !extra
                else
                  ()
            ) ;
            iter tail
        | ((Sexp.Atom _ | Sexp.List _) as sexp) :: _ ->
            Sexp_conv_error.record_only_pairs_expected !my_name sexp
        | [] ->
            ()
      in
      iter field_sexps ;
      match (!duplicates, !extra) with
      | _ :: _, _ ->
          Sexp_conv_error.record_duplicate_fields !my_name !duplicates sexp
      | [], _ :: _ ->
          Sexp_conv_error.record_extra_fields !my_name !extra sexp
      | [], [] -> (
        match (!process_field, !filename_field, !line_field) with
        | Some process_value, Some filename_value, Some line_value ->
            {process= process_value; filename= filename_value; line= line_value}
        | _ ->
            Sexp_conv_error.record_undefined_elements !my_name sexp
              [
                (!process_field = None, "process")
              ; (!filename_field = None, "filename")
              ; (!line_field = None, "line")
              ]
      )
    )
  | Sexp.Atom _ as sexp ->
      Sexp_conv_error.record_list_instead_atom !my_name sexp

let sexp_of_frame {process= v_process; filename= v_filename; line= v_line} =
  let open Sexplib0 in
  let open Sexplib0.Sexp_conv in
  let process_value = sexp_of_string v_process in
  let filename_value = sexp_of_string v_filename in
  let line_value = sexp_of_int v_line in
  Sexp.List
    [
      Sexp.List [Sexp.Atom "process"; process_value]
    ; Sexp.List [Sexp.Atom "filename"; filename_value]
    ; Sexp.List [Sexp.Atom "line"; line_value]
    ]

let t_of_sexp = Sexplib0.Sexp_conv.list_of_sexp frame_of_sexp

let sexp_of_t = Sexplib0.Sexp_conv.sexp_of_list sexp_of_frame

let to_string_hum xs =
  let xs' = List.length xs in
  let results = Buffer.create 10 in
  let rec loop first_line i = function
    | [] ->
        Buffer.contents results
    | x :: xs ->
        Buffer.add_string results
          (Printf.sprintf "%d/%d %s %s file %s, line %d" i xs' x.process
             (if first_line then "Raised at" else "Called from")
             x.filename x.line) ;
        Buffer.add_string results "\n" ;
        loop false (i + 1) xs
  in
  loop true 1 xs

type table = {
    backtraces: t array
  ; exn_to_backtrace: exn Weak.t
  ; mutable producer: int
  ; (* free running counter *)
    m: Mutex.t
}

(* Increasing this makes 'find_all' slower and increases the amount of
   memory needed. Since we have a table per thread a small number should
   be enough. *)
let max_backtraces = 100

let frame_of_string process x =
  try
    match split_c '"' x with
    | [_; filename; rest] -> (
      match split_c ',' rest with
      | [_; line_n; _] -> (
        match split_c ' ' line_n with
        | _ :: _ :: n :: _ ->
            {process; filename; line= int_of_string n}
        | _ ->
            failwith (Printf.sprintf "Failed to parse line: [%s]" line_n)
      )
      | _ ->
          failwith (Printf.sprintf "Failed to parse fragment: [%s]" filename)
    )
    | _ ->
        failwith (Printf.sprintf "Failed to parse fragment: [%s]" x)
  with e -> {process; filename= "(" ^ Printexc.to_string e ^ ")"; line= 0}

let get_backtrace_401 () =
  Printexc.get_backtrace ()
  |> split_c '\n'
  |> List.filter (fun x -> x <> "")
  |> List.map (frame_of_string !my_name)

let make () =
  let backtraces = Array.make max_backtraces [] in
  let exn_to_backtrace = Weak.create max_backtraces in
  let producer = 0 in
  (* free running *)
  let m = Mutex.create () in
  {backtraces; exn_to_backtrace; producer; m}

let add t exn bt =
  Mutex.execute t.m (fun () ->
      let slot = t.producer mod max_backtraces in
      t.producer <- t.producer + 1 ;
      Weak.set t.exn_to_backtrace slot (Some exn) ;
      t.backtraces.(slot) <- bt)

let is_important t exn =
  let bt = get_backtrace_401 () in
  (* Deliberately clear the backtrace buffer *)
  (try raise Not_found with Not_found -> ()) ;
  add t exn bt

(* fold over the slots matching exn *)
let fold t exn f initial =
  let rec loop acc from =
    if from < 0 || t.producer - from > max_backtraces then
      acc
    else
      let slot = from mod max_backtraces in
      match Weak.get t.exn_to_backtrace slot with
      | Some exn' when exn' == exn ->
          loop (f acc slot) (from - 1)
      | _ ->
          loop acc (from - 1)
  in
  loop initial (t.producer - 1)

let remove_dups xs =
  List.fold_left
    (fun (last, acc) item -> (item, if last = item then acc else item :: acc))
    ([], []) (List.rev xs)
  |> snd

(*
  |> List.rev
*)
let get t exn =
  fold t exn (fun acc slot -> t.backtraces.(slot) :: acc) []
  |> remove_dups
  |> List.concat

let remove t exn =
  fold t exn
    (fun acc slot ->
      let bt = t.backtraces.(slot) in
      Weak.set t.exn_to_backtrace slot None ;
      t.backtraces.(slot) <- [] ;
      bt :: acc)
    []
  |> remove_dups
  |> List.concat

let per_thread_backtraces = Hashtbl.create 37

let per_thread_backtraces_m = Mutex.create ()

let with_lock f x =
  Mutex.lock per_thread_backtraces_m ;
  try
    let result = f x in
    Mutex.unlock per_thread_backtraces_m ;
    result
  with e ->
    Mutex.unlock per_thread_backtraces_m ;
    raise e

let with_backtraces f =
  let id = Thread.(id (self ())) in
  let tbl =
    with_lock
      (fun () ->
        let tbl =
          if Hashtbl.mem per_thread_backtraces id then
            Hashtbl.find per_thread_backtraces id
          else
            make ()
        in
        (* If we nest these functions we add multiple bindings
           to the same mutable table which is ok *)
        Hashtbl.add per_thread_backtraces id tbl ;
        tbl)
      ()
  in
  try
    let result = f () in
    with_lock (Hashtbl.remove per_thread_backtraces) id ;
    `Ok result
  with e ->
    let bt = get tbl e in
    with_lock (Hashtbl.remove per_thread_backtraces) id ;
    `Error (e, bt)

let with_table f default =
  let id = Thread.(id (self ())) in
  match
    with_lock
      (fun () ->
        if Hashtbl.mem per_thread_backtraces id then
          Some (Hashtbl.find per_thread_backtraces id)
        else
          None)
      ()
  with
  | None ->
      default ()
  | Some tbl ->
      f tbl

let is_important exn =
  with_table (fun tbl -> is_important tbl exn) (fun () -> ())

let add exn bt = with_table (fun tbl -> add tbl exn bt) (fun () -> ())

let warning () =
  [
    {
      process= !my_name
    ; filename=
        Printf.sprintf
          "(Thread %d has no backtrace table. Was with_backtraces called?"
          Thread.(id (self ()))
    ; line= 0
    }
  ]

let remove exn = with_table (fun tbl -> remove tbl exn) warning

let get exn = with_table (fun tbl -> get tbl exn) warning

let reraise old newexn =
  add newexn (remove old) ;
  raise newexn

module Interop = struct
  (* This matches xapi.py:exception *)
  type error = {
      error: string
    ; (* Python json.dumps and rpclib are not very friendly *)
      files: string list
    ; lines: int list
  }
  [@@deriving rpc]

  let of_json source_name txt =
    txt |> Jsonrpc.of_string |> error_of_rpc |> fun e ->
    List.combine e.files e.lines
    |> List.map (fun (filename, line) -> {process= source_name; filename; line})
end
