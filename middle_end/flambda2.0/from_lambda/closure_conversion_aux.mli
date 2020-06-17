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

(** Environments and auxiliary structures used during closure conversion. *)

(** Used to remember which [Variable.t] values correspond to which
    [Ident.t] values during closure conversion, and similarly for
     static exception identifiers. *)
module Env : sig
  type t

  val empty : t

  val clear_local_bindings : t -> t

  val add_var : t -> Ident.t -> Variable.t -> t
  val add_vars : t -> Ident.t list -> Variable.t list -> t
  val add_var_map : t -> Variable.t Ident.Map.t -> t

  val add_var_like : t -> Ident.t -> Ilambda.user_visible -> t * Variable.t

  val add_vars_like
     : t
    -> (Ident.t * Ilambda.user_visible) list
    -> t * Variable.t list

  val find_name : t -> Ident.t -> Name.t
  val find_name_exn : t -> Ident.t -> Name.t

  val find_var : t -> Ident.t -> Variable.t
  val find_var_exn : t -> Ident.t -> Variable.t

  val find_vars : t -> Ident.t list -> Variable.t list

  val add_global : t -> int -> Symbol.t -> t
  val find_global : t -> int -> Symbol.t

  val add_simple_to_substitute : t -> Ident.t -> Simple.t -> t

  val find_simple_to_substitute_exn : t -> Ident.t -> Simple.t

  val still_at_toplevel : t -> bool
  val no_longer_at_toplevel : t -> t
end

(** Used to represent information about a set of function declarations
    during closure conversion.  (The only case in which such a set may
    contain more than one declaration is when processing "let rec".) *)
module Function_decls : sig
  module Function_decl : sig
    type t

    val create
       : let_rec_ident:Ident.t option
      -> closure_id:Closure_id.t
      -> kind:Lambda.function_kind
      -> params:(Ident.t * Lambda.value_kind) list
      -> return:Lambda.value_kind
      -> return_continuation:Continuation.t
      -> exn_continuation:Ilambda.exn_continuation
      -> body:Ilambda.t
      -> attr:Lambda.function_attribute
      -> loc:Lambda.scoped_location
      -> free_idents_of_body:Ident.Set.t
      -> stub:bool
      -> Recursive.t
      -> t

    val let_rec_ident : t -> Ident.t
    val closure_id : t -> Closure_id.t
    val kind : t -> Lambda.function_kind
    val params : t -> (Ident.t * Lambda.value_kind) list
    val return : t -> Lambda.value_kind
    val return_continuation : t -> Continuation.t
    val exn_continuation : t -> Ilambda.exn_continuation
    val body : t -> Ilambda.t
    val inline : t -> Lambda.inline_attribute
    val specialise : t -> Lambda.specialise_attribute
    val is_a_functor : t -> bool
    val stub : t -> bool
    val loc : t -> Lambda.scoped_location
    val recursive : t -> Recursive.t
    val contains_closures : t -> bool

    (* Like [all_free_idents], but for just one function. *)
    val free_idents : t -> Ident.Set.t
  end

  type t

  val create : Function_decl.t list -> t
  val to_list : t -> Function_decl.t list

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> Ident.Set.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are not bound as parameters.
     It also contains the globals bindings of the provided environment. *)
  (* val closure_env_without_parameters : Env.t -> t -> Env.t *)
end
