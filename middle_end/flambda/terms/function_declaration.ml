(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  code_id : Code_id.t;
  params_arity : Flambda_arity.t;
  result_arity : Flambda_arity.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
  is_a_functor : bool;
  recursive : Recursive.t;
  is_tupled : bool;
}

let invariant _env t =
  (* check arity of tupled functions *)
  if t.is_tupled then begin
    match t.params_arity with
    | [Value] -> ()
    | _ ->
      Misc.fatal_errorf "tupled functions must take a single value as argument"
  end

let create ~code_id ~params_arity ~result_arity ~stub ~dbg
      ~(inline : Inline_attribute.t)
      ~is_a_functor ~recursive ~is_tupled : t =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_error "Stubs may not be annotated as [Always_inline] or [Unroll]"
  end;
  { code_id;
    params_arity;
    result_arity;
    stub;
    dbg;
    inline;
    is_a_functor;
    recursive;
    is_tupled;
  }

let print_with_cache ~cache:_ ppf
      { code_id;
        params_arity;
        result_arity;
        stub;
        dbg;
        inline;
        is_a_functor;
        recursive;
        is_tupled;
      } =
  let module C = Flambda_colours in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(code_id@ %a)@]@ \
      @[<hov 1>@<0>%s(stub@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(inline@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(is_a_functor@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(params_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>@<0>%s(result_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>@<0>%s(recursive@ %a)@<0>%s@] \
      @[<hov 1>@<0>%s(is_tupled@ %b)@<0>%s@])"
    Code_id.print code_id
    (if not stub then Flambda_colours.elide () else C.normal ())
    stub
    (Flambda_colours.normal ())
    (Flambda_colours.debuginfo ())
    Debuginfo.print_compact dbg
    (Flambda_colours.normal ())
    (if Inline_attribute.is_default inline
     then Flambda_colours.elide ()
     else C.normal ())
    Inline_attribute.print inline
    (Flambda_colours.normal ())
    (if not is_a_functor then Flambda_colours.elide () else C.normal ())
    is_a_functor
    (Flambda_colours.normal ())
    (if Flambda_arity.is_singleton_value params_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.print params_arity
    (if Flambda_arity.is_singleton_value params_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    (if Flambda_arity.is_singleton_value result_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.print result_arity
    (if Flambda_arity.is_singleton_value result_arity
     then Flambda_colours.elide ()
     else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    (match recursive with
     | Non_recursive -> Flambda_colours.elide ()
     | Recursive -> Flambda_colours.normal ())
    Recursive.print recursive
    (Flambda_colours.normal ())
    (if is_tupled
     then Flambda_colours.normal ()
     else Flambda_colours.elide ())
    is_tupled
    (Flambda_colours.normal ())

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let code_id t = t.code_id
let params_arity t = t.params_arity
let result_arity t = t.result_arity
let stub t = t.stub
let dbg t = t.dbg
let inline t = t.inline
let is_a_functor t = t.is_a_functor
let recursive t = t.recursive
let is_tupled t = t.is_tupled

let free_names
      { code_id;
        params_arity = _;
        result_arity = _;
        stub = _;
        dbg = _;
        inline = _;
        is_a_functor = _;
        recursive = _;
        is_tupled = _;
      } =
  Name_occurrences.add_code_id Name_occurrences.empty code_id Name_mode.normal

let apply_name_permutation t _perm = t

let all_ids_for_export
      { code_id;
        params_arity = _;
        result_arity = _;
        stub = _;
        dbg = _;
        inline = _;
        is_a_functor = _;
        recursive = _;
        is_tupled = _;
      } =
  Ids_for_export.add_code_id Ids_for_export.empty code_id

let import import_map
      { code_id;
        params_arity;
        result_arity;
        stub;
        dbg;
        inline;
        is_a_functor;
        recursive;
        is_tupled;
      } =
  let code_id = Ids_for_export.Import_map.code_id import_map code_id in
  { code_id;
    params_arity;
    result_arity;
    stub;
    dbg;
    inline;
    is_a_functor;
    recursive;
    is_tupled;
  }


let update_code_id t code_id = { t with code_id; }

(* CR mshinwell: In the "equal" case this should assert that all of the
   other things in [t1] and [t2] are equal *)
let compare t1 t2 =
  Code_id.compare t1.code_id t2.code_id

let equal t1 t2 = (compare t1 t2 = 0)
