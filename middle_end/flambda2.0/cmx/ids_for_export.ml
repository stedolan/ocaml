(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Const = Reg_width_things.Const

type t = {
  symbols : Symbol.Set.t;
  variables : Variable.Set.t;
  simples : Simple.Set.t;
  consts : Const.Set.t;
  code_ids : Code_id.Set.t;
}

let empty = {
  symbols = Symbol.Set.empty;
  variables = Variable.Set.empty;
  simples = Simple.Set.empty;
  consts = Const.Set.empty;
  code_ids = Code_id.Set.empty;
}

let create
    ?(symbols = Symbol.Set.empty)
    ?(variables = Variable.Set.empty)
    ?(simples = Simple.Set.empty)
    ?(consts = Const.Set.empty)
    ?(code_ids = Code_id.Set.empty)
    () =
  { symbols;
    variables;
    simples;
    consts;
    code_ids;
  }

let add_const t const =
  { t with consts = Const.Set.add const t.consts }

let add_variable t var =
  { t with variables = Variable.Set.add var t.variables }

let add_symbol t sym =
  { t with symbols = Symbol.Set.add sym t.symbols }

let add_name t name =
  Name.pattern_match name
    ~var:(add_variable t)
    ~symbol:(add_symbol t)

let add_simple t simple =
  let simples =
    match Simple.rec_info simple with
    | None -> t.simples
    | Some _ -> Simple.Set.add simple t.simples
  in
  let t = { t with simples; } in
  Simple.pattern_match simple
    ~const:(add_const t)
    ~name:(add_name t)

let add_code_id t code_id =
  { t with code_ids = Code_id.Set.add code_id t.code_ids }

let from_simple simple =
  let simples =
    match Simple.rec_info simple with
    | None ->
      (* This simple will not be in the grand_table_of_simples *)
      Simple.Set.empty
    | Some _ -> Simple.Set.singleton simple
  in
  Simple.pattern_match simple
    ~const:(fun const ->
      create ~simples
        ~consts:(Const.Set.singleton const)
        ())
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun var ->
          create ~simples
            ~variables:(Variable.Set.singleton var)
            ())
        ~symbol:(fun sym ->
          create ~simples
            ~symbols:(Symbol.Set.singleton sym)
            ()))

let union t1 t2 =
  { symbols = Symbol.Set.union t1.symbols t2.symbols;
    variables = Variable.Set.union t1.variables t2.variables;
    simples = Simple.Set.union t1.simples t2.simples;
    consts = Const.Set.union t1.consts t2.consts;
    code_ids = Code_id.Set.union t1.code_ids t2.code_ids;
  }

module Import_map = struct
  type t = {
    symbols : Symbol.t Symbol.Map.t;
    variables : Variable.t Variable.Map.t;
    simples : Simple.t Simple.Map.t;
    consts : Const.t Const.Map.t;
    code_ids : Code_id.t Code_id.Map.t;
  }

  let create ~symbols ~variables ~simples ~consts ~code_ids =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
    }

  let symbol t orig =
    match Symbol.Map.find orig t.symbols with
    | symbol -> symbol
    | exception Not_found -> orig

  let variable t orig =
    match Variable.Map.find orig t.variables with
    | variable -> variable
    | exception Not_found -> orig

  let const t orig =
    match Const.Map.find orig t.consts with
    | const -> const
    | exception Not_found -> orig

  let code_id t orig =
    match Code_id.Map.find orig t.code_ids with
    | code_id -> code_id
    | exception Not_found -> orig

  let name t name =
    Name.pattern_match name
      ~var:(fun var -> Name.var (variable t var))
      ~symbol:(fun sym -> Name.symbol (symbol t sym))

  let simple t simple =
    match Simple.Map.find simple t.simples with
    | simple -> simple
    | exception Not_found ->
      begin match Simple.rec_info simple with
      | None ->
        Simple.pattern_match simple
          ~name:(fun n -> Simple.name (name t n))
          ~const:(fun c -> Simple.const (const t c))
      | Some _rec_info -> simple
      end
end
