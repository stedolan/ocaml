(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

let raise_count = ref 0

let next_raise_count () =
(*
if !raise_count = 52 then begin
Format.eprintf "Creation of continuation %d:\n%s\n%!"
  (!raise_count + 1)
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100))
end;
*)
  incr raise_count;
  !raise_count

module Sort = struct
  type t =
    | Normal
    | Return
    | Define_root_symbol
    | Toplevel_return
    | Exn

  let to_int t =
    match t with
    | Normal -> 0
    | Return -> 1
    | Define_root_symbol -> 2
    | Toplevel_return -> 3
    | Exn -> 4

  let of_int t =
    match t with
    | 0 -> Normal
    | 1 -> Return
    | 2 -> Define_root_symbol
    | 3 -> Toplevel_return
    | 4 -> Exn
    | _ -> assert false

  let num_bits_needed = 3
end

let max_num_continuations = (-1) lsr Sort.num_bits_needed
let () = assert (max_num_continuations > 0)

let sort_shift = Sys.int_size - Sort.num_bits_needed

(* There is no need to store the compilation unit, since in a .cmx file,
   no values of type [t] occur that are not under [Name_abstraction] binders. *)
type t = int

let create ?sort () : t =
  let sort = Option.value sort ~default:Sort.Normal in
  let id = next_raise_count () in
  if id < max_num_continuations then
    ((Sort.to_int sort) lsl sort_shift) lor id
  else
    Misc.fatal_error "Use a machine with a wider word size to compile"

(* CR mshinwell: Document why this uses [next_raise_count].  Does it need
   to?  It would be better if it didn't. *)

let dummy = create ~sort:Normal ()

let to_int t = t land max_num_continuations
let sort t = Sort.of_int (t lsr sort_shift)

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    (* The [id] uniquely determines the [sort], so there's no need to look
       at the latter. *)
    Int.compare t1 t2

  let equal t1 t2 =
    t1 == t2

  let hash t =
    Hashtbl.hash t

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.continuation ());
    Format.fprintf ppf "k%d" (to_int t);
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

module Set = Patricia_tree.Make_set (struct let print = print end)
module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
(* CR mshinwell: The [Tbl]s will still print integers! *)
module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

let print_with_cache ~cache:_ ppf t = print ppf t

module With_args = struct
  type nonrec t = t * Variable.t list

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      let c = compare (fst t1) (fst t2) in
      if c <> 0 then c
      else Variable.compare_lists (snd t1) (snd t2)

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =
      Hashtbl.hash (hash (fst t),
        List.map Variable.hash (snd t))

    let print ppf (cont, vars) =
      Format.fprintf ppf "@[(%a, %a)@]"
        print cont
        Variable.print_list vars

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)
end

let is_exn t =
  match sort t with
  | Exn -> true
  | Normal
  | Return
  | Define_root_symbol
  | Toplevel_return -> false
