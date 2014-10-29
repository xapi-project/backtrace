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
open Sexplib.Std

let my_name = ref (Filename.basename Sys.argv.(0))
let set_my_name x = my_name := x

module Mutex = struct
  include Mutex
  (** execute the function f with the mutex hold *)
  let execute lock f =
    Mutex.lock lock;
    let r = begin try f () with exn -> Mutex.unlock lock; raise exn end; in
    Mutex.unlock lock;
    r
end

let rec split_c c str =
  try
    let i = String.index str c in
    String.sub str 0 i :: (split_c c (String.sub str (i+1) (String.length str - i - 1)))
  with Not_found -> [str]
    
type t = string list (* < OCaml 4.02.0 *)
  with sexp

let empty = []

let to_string_hum xs =
  let xs' = List.length xs in
  let results = Buffer.create 10 in
  let rec loop i = function
  | [] -> Buffer.contents results
  | x :: xs ->
    Buffer.add_string results (Printf.sprintf "%d/%d %s" i xs' x);
    Buffer.add_string results "\n";
    loop (i + 1) xs in
  loop 0 xs

type table = {
  backtraces: t array;
  exn_to_backtrace: exn Weak.t;
  mutable producer: int; (* free running counter *)
  m: Mutex.t;
}

(* Increasing this makes 'find_all' slower and increases the amount of
   memory needed. We maybe should make this a thread-local table. *)
let max_backtraces = 100

let get_backtrace_401 () =
  Printexc.get_backtrace ()
  |> split_c '\n'
  |> List.filter (fun x -> x <> "")
  |> List.map (fun x -> !my_name ^ " " ^ x)

let make () =
  let backtraces = Array.make max_backtraces [] in
  let exn_to_backtrace = Weak.create max_backtraces in
  let producer = 0 in (* free running *)
  let m = Mutex.create () in
  { backtraces; exn_to_backtrace; producer; m }

let add t exn bt =
  Mutex.execute t.m
    (fun () ->
      let slot = t.producer mod max_backtraces in
      t.producer <- t.producer + 1;
      Weak.set t.exn_to_backtrace slot (Some exn);
      t.backtraces.(slot) <- bt;
    )

let is_important t exn =
  let bt = get_backtrace_401 () in
  (* Deliberately clear the backtrace buffer *)
  (try raise Not_found with Not_found -> ());
  add t exn bt

(* fold over the slots matching exn *)
let fold t exn f initial =
  let rec loop acc from =
    if from < 0 || t.producer - from > max_backtraces
    then acc
    else
      let slot = from mod max_backtraces in
      match Weak.get t.exn_to_backtrace slot with
      | Some exn' when exn' == exn ->
        loop (f acc slot) (from - 1)
      | _ ->
        loop acc (from - 1) in
  loop initial (t.producer - 1)

let remove_dups xs =
  List.fold_left (fun (last, acc) item ->
    item, (if last = item then acc else item :: acc)
  ) ([], []) (List.rev xs)
  |> snd
(*
  |> List.rev
*)
let get t exn =
  fold t exn (fun acc slot -> t.backtraces.(slot) :: acc) [] |> remove_dups |> List.concat

let remove t exn =
  fold t exn (fun acc slot ->
      let bt = t.backtraces.(slot) in
      Weak.set t.exn_to_backtrace slot None;
      t.backtraces.(slot) <- [];
      bt :: acc
  ) [] |> remove_dups |> List.concat


let global_backtraces = make ()

let is_important = is_important global_backtraces

let add = add global_backtraces

let remove = remove global_backtraces

let get = get global_backtraces

let reraise old newexn =
  add newexn (remove old);
  raise newexn
