(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of dynamically-linked libraries *)

type dll_handle
type dll_address

external dll_open: string -> dll_handle = "caml_dynlink_open_lib"
external dll_sym: dll_handle -> string -> dll_address
                = "caml_dynlink_lookup_symbol"
         (* returned dll_address may be Val_unit *)
external add_primitive: dll_address -> int = "caml_dynlink_add_primitive"
external get_current_dlls: unit -> dll_handle array
                                           = "caml_dynlink_get_current_libs"

(* Current search path for DLLs *)
let search_path = ref ([] : string list)

(* DLLs currently opened *)
let opened_dlls = ref ([] : (string * dll_handle) list)

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)

let extract_dll_name file =
  if Filename.check_suffix file Config.ext_dll then
    Filename.chop_suffix file Config.ext_dll
  else if String.length file >= 2 && String.sub file 0 2 = "-l" then
    "dll" ^ String.sub file 2 (String.length file - 2)
  else
    file (* will cause error later *)

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)

let find_in_path path name =
  if not (Filename.is_implicit name) then
    if Sys.file_exists name then name else raise Not_found
  else begin
    let rec try_dir = function
      [] -> raise Not_found
    | dir::rem ->
        let fullname = Filename.concat dir name in
        if Sys.file_exists fullname then fullname else try_dir rem
    in try_dir path
  end

let open_dll name =
  let name = name ^ Config.ext_dll in
  let fullname =
    try
      let fullname = find_in_path !search_path name in
      if Filename.is_implicit fullname then
        Filename.concat Filename.current_dir_name fullname
      else fullname
    with Not_found -> name in
  match List.assoc_opt fullname !opened_dlls with
  | Some _ -> ()
  | None ->
      begin match dll_open fullname with
      | dll ->
          opened_dlls := (fullname, dll) :: !opened_dlls
      | exception Failure msg ->
          failwith (fullname ^ ": " ^ msg)
      end

let open_dlls names =
  List.iter open_dll names


(* Find a primitive in the currently opened DLLs. *)

let find_primitive prim_name =
  let rec find seen = function
    [] ->
      None
  | (_, dll) as curr :: rem ->
      let addr = dll_sym dll prim_name in
      if addr == Obj.magic () then find (curr :: seen) rem else begin
        if seen <> [] then opened_dlls := curr :: List.rev_append seen rem;
        Some addr
      end
  in
  find [] !opened_dlls

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)

let synchronize_primitive num symb =
  let actual_num = add_primitive symb in
  assert (actual_num = num)

(* Initialization for linking in core (dynlink or toplevel) *)

let init_toplevel dllpaths =
  search_path := dllpaths;
  opened_dlls :=
    List.map (fun dll -> "", dll)
      (Array.to_list (get_current_dlls()))

