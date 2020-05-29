(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t
  val is_empty : t -> bool

  val add : t -> Simple.t -> Name_mode.t -> t

  val find_earliest_candidates_exn
     : t
    -> min_name_mode:Name_mode.t
    -> Simple.Set.t

  val all : t -> Simple.Set.t

  val mem : t -> Simple.t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t
end = struct
  type t = {
    aliases : Simple.Set.t Name_mode.Map.t;
    all : Simple.Set.t;
  }

  let invariant _t = ()

  let print ppf { aliases; all = _; } =
    Name_mode.Map.print Simple.Set.print ppf aliases

  let empty = {
    aliases = Name_mode.Map.empty;
    all = Simple.Set.empty;
  }

  let is_empty t = Simple.Set.is_empty t.all

  let add t elt name_mode =
    if Simple.Set.mem elt t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
          %a"
        Simple.print elt
        print t
    end;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Simple.Set.singleton elt)
          | Some elts ->
            if !Clflags.flambda_invariant_checks then begin
              assert (not (Simple.Set.mem elt elts))
            end;
            Some (Simple.Set.add elt elts))
        t.aliases
    in
    let all = Simple.Set.add elt t.all in
    { aliases;
      all;
    }

  let find_earliest_candidates_exn t ~min_name_mode =
    let _order, elts =
      Name_mode.Map.find_first (fun order ->
          match
            Name_mode.compare_partial_order
              order min_name_mode
          with
          | None -> false
          | Some result -> result >= 0)
        t.aliases
    in
    elts

  let mem t elt =
    Simple.Set.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases =
      Name_mode.Map.union (fun _order elts1 elts2 ->
          Some (Simple.Set.union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = Simple.Set.union t1.all t2.all;
      }
    in
    invariant t;
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge (fun _order elts1 elts2 ->
          match elts1, elts2 with
          | None, None | Some _, None | None, Some _ -> None
          | Some elts1, Some elts2 -> Some (Simple.Set.inter elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = Simple.Set.inter t1.all t2.all;
      }
    in
    invariant t;
    t
end

type t = {
  canonical_elements : Simple.t Simple.Map.t;
  (* Canonical elements that have no known aliases are not included in
     [canonical_elements]. *)
  aliases_of_canonical_elements : Aliases_of_canonical_element.t Simple.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_elements], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  binding_times_and_modes : Binding_time.With_name_mode.t Simple.Map.t;
  (* Binding times and name modes define an order on the elements.
     The canonical element for a set of aliases is always the minimal
     element for this order, which is different from the order used
     for creating sets and maps. *)
}

let print ppf { canonical_elements; aliases_of_canonical_elements;
                binding_times_and_modes; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_elements@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_elements@ %a)@]\
      @[<hov 1>(binding_times_and_modes@ %a)@]\
      )@]"
    (Simple.Map.print Simple.print) canonical_elements
    (Simple.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_elements
    (Simple.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let defined_earlier t alias ~than =
  let info1 = Simple.Map.find alias t.binding_times_and_modes in
  let info2 = Simple.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let name_mode t elt =
  Binding_time.With_name_mode.name_mode
    (Simple.Map.find elt t.binding_times_and_modes)

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let _all_aliases : Simple.Set.t =
      Simple.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Simple.Set.for_all (fun elt ->
            defined_earlier t canonical_element ~than:elt) aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Simple.print canonical_element
              print t
          end;
          if Simple.Set.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Simple.print canonical_element
              Simple.Set.print aliases
          end;
          if not (Simple.Set.is_empty (Simple.Set.inter aliases all_aliases)) then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Simple.Set.union aliases all_aliases)
        t.aliases_of_canonical_elements
        Simple.Set.empty
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Simple.Map.empty;
  aliases_of_canonical_elements = Simple.Map.empty;
  binding_times_and_modes = Simple.Map.empty;
}

type canonical =
  | Is_canonical of Simple.t
  | Alias_of_canonical of { element : Simple.t; canonical_element : Simple.t; }

let canonical t element : canonical =
  match Simple.Map.find element t.canonical_elements with
  | exception Not_found -> Is_canonical element
  | canonical_element ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal element canonical_element))
    end;
    Alias_of_canonical { element; canonical_element; }

let get_aliases_of_canonical_element t ~canonical_element =
  match Simple.Map.find canonical_element t.aliases_of_canonical_elements with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

let add_alias_between_canonical_elements t ~canonical_element ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted then
    t
  else
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_to_be_demoted canonical_element))
    end;
    let canonical_elements =
      t.canonical_elements
      |> Simple.Set.fold (fun alias canonical_elements ->
          Simple.Map.add alias canonical_element canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Simple.Map.add to_be_demoted canonical_element
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element to_be_demoted));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted))
    end;
    let aliases =
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union aliases_of_to_be_demoted
          aliases_of_canonical_element)
        to_be_demoted (name_mode t to_be_demoted)
    in
    let aliases_of_canonical_elements =
      t.aliases_of_canonical_elements
      |> Simple.Map.remove to_be_demoted
      |> Simple.Map.add (* replace *) canonical_element aliases
    in
    { canonical_elements;
      aliases_of_canonical_elements;
      binding_times_and_modes = t.binding_times_and_modes;
    }

type to_be_demoted = {
  canonical_element : Simple.t;
  to_be_demoted : Simple.t;
}

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then
    { canonical_element = canonical_element1;
      to_be_demoted = canonical_element2;
    }
  else
    { canonical_element = canonical_element2;
      to_be_demoted = canonical_element1;
    }

(* CR mshinwell: add submodule *)
type add_result = {
  t : t;
  canonical_element : Simple.t;
  alias_of : Simple.t;
}

let invariant_add_result ~original_t { canonical_element; alias_of; t; } =
  if !Clflags.flambda_invariant_checks then begin
    invariant t;
    if not (Simple.equal canonical_element alias_of) then begin
      if not (defined_earlier t canonical_element ~than:alias_of) then begin
        Misc.fatal_errorf "Canonical element %a should be defined earlier \
            than %a after alias addition.@ Original alias tracker:@ %a@ \
            Resulting alias tracker:@ %a"
          Simple.print canonical_element
          Simple.print alias_of
          print original_t
          print t
      end
    end
  end

let add_alias t element1 element2 =
  match canonical t element1, canonical t element2 with
  | Is_canonical canonical_element1, Is_canonical canonical_element2 ->
    let { canonical_element; to_be_demoted; } =
      choose_canonical_element_to_be_demoted t ~canonical_element1
        ~canonical_element2
    in
    let t =
      add_alias_between_canonical_elements t ~canonical_element
        ~to_be_demoted
    in
    { t;
      canonical_element;
      (* CR mshinwell: [alias_of] is not a good name. *)
      alias_of = to_be_demoted;
    }
  | Alias_of_canonical
        { element = element1; canonical_element = canonical_element1; },
      Is_canonical canonical_element2
  | Is_canonical canonical_element2,
      Alias_of_canonical
        { element = element1; canonical_element = canonical_element1; } ->
    let { canonical_element; to_be_demoted; } =
      choose_canonical_element_to_be_demoted t ~canonical_element1
        ~canonical_element2
    in
    let alias_of =
      if Simple.equal to_be_demoted canonical_element1 then element1
      else canonical_element2
    in
    let t =
      add_alias_between_canonical_elements t ~canonical_element
        ~to_be_demoted
    in
    { t;
      canonical_element;
      alias_of;
    }
  | Alias_of_canonical
        { element = element1; canonical_element = canonical_element1; },
      Alias_of_canonical
        { element = element2; canonical_element = canonical_element2; }
      ->
    let { canonical_element; to_be_demoted; } =
      choose_canonical_element_to_be_demoted t ~canonical_element1
        ~canonical_element2
    in
    let alias_of =
      if Simple.equal to_be_demoted canonical_element1 then element1
      else element2
    in
    let t =
      add_alias_between_canonical_elements t ~canonical_element
        ~to_be_demoted
    in
    { t;
      canonical_element;
      alias_of;
    }

let add t element1 binding_time_and_mode1
      element2 binding_time_and_mode2 =
  let original_t = t in
  let element1 = Simple.without_rec_info element1 in
  let element2 = Simple.without_rec_info element2 in
  let t =
    { t with binding_times_and_modes =
               Simple.Map.add element1 binding_time_and_mode1
                 (Simple.Map.add element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result = add_alias t element1 element2 in
  if !Clflags.flambda_invariant_checks then begin
    invariant_add_result ~original_t add_result
  end;
  add_result

  (* CR mshinwell: This needs documenting.  For the moment we allow
     relations between canonical elements that are actually incomparable
     under the name mode ordering, and check in [get_canonical_element_exn]
     accordingly.  However maybe we should never allow these situations to
     arise. *)
  (*
  let canonical_mode =
    name_mode t add_result.canonical_element
  in
  let alias_of_mode = name_mode t add_result.alias_of in
  match
    Name_mode.compare_partial_order
      canonical_mode alias_of_mode
  with
  | Some _ -> add_result
  | None ->
    Misc.fatal_errorf "Canonical %a has mode incomparable with %a in:@ %a"
      Simple.print add_result.canonical_element
      Simple.print add_result.alias_of
      print t
  *)

let get_canonical_element_exn t element elt_name_mode ~min_name_mode =
  match Simple.Map.find element t.canonical_elements with
  | exception Not_found ->
    begin match
      Name_mode.compare_partial_order elt_name_mode min_name_mode
    with
    | None -> raise Not_found
    | Some c ->
      if c >= 0 then element
      else raise Not_found
    end
  | canonical_element ->
  (*
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  Simple.print element
  Simple.print canonical_element
  Name_mode.print min_name_mode;
*)
    let find_earliest () =
      let aliases = get_aliases_of_canonical_element t ~canonical_element in
      let at_earliest_mode =
        (* May raise Not_found if no aliases exist at a mode compatible with
           min_name_mode *)
        Aliases_of_canonical_element.find_earliest_candidates_exn aliases
          ~min_name_mode
      in
      (* Aliases_of_canonical_element.find_earliest_candidates_exn only returns
         non-empty sets *)
      assert (not (Simple.Set.is_empty at_earliest_mode));
      Simple.Set.fold (fun elt min_elt ->
          if defined_earlier t elt ~than:min_elt
          then elt
          else min_elt)
        at_earliest_mode
        (Simple.Set.min_elt at_earliest_mode)
    in
    match
      Name_mode.compare_partial_order
        (name_mode t canonical_element)
        min_name_mode
    with
    | None -> find_earliest ()
    | Some c ->
      if c >= 0 then canonical_element
      else find_earliest ()

let get_aliases t element =
  match canonical t element with
  | Is_canonical canonical_element ->
    let aliases =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    Simple.Set.add element aliases
  | Alias_of_canonical { element = _; canonical_element; } ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal element canonical_element))
    end;
    let aliases =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (Simple.Set.mem element aliases)
    end;
    Simple.Set.add canonical_element aliases

let get_canonical_ignoring_name_mode t name =
  let simple = Simple.name name in
  match canonical t simple with
  | Is_canonical _ -> simple
  | Alias_of_canonical { canonical_element; _ } -> canonical_element
