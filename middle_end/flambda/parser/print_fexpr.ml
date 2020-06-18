open Fexpr

let pp_space_list f ppf =
  Format.pp_print_list f
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
    ppf


let pp_semi_list f ppf =
  Format.pp_print_list f
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ";")
    ppf

let pp_comma_list f ppf =
  Format.pp_print_list f
    ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
    ppf

let recursive ppf = function
  | Nonrecursive -> ()
  | Recursive -> Format.fprintf ppf "@ rec"

let is_stub ppf = function
  | false -> ()
  | true -> Format.fprintf ppf "@ stub"

let is_exn ppf = function
  | false -> ()
  | true -> Format.fprintf ppf "@ exn"

let csymbol ppf (s, _loc) =
  Format.fprintf ppf "%s" s

let symbol ppf (s, _loc) =
  Format.fprintf ppf "%s" s

let variable ppf (s, _loc) =
  Format.fprintf ppf "%s" s

let variable_opt ppf s =
  match s with
  | None -> Format.fprintf ppf "_"
  | Some (s, _loc) -> Format.fprintf ppf "%s" s

let continuation ppf (s, _loc) =
  Format.fprintf ppf "%s" s

let exn_continuation ppf c =
  Format.fprintf ppf " * %a" continuation c

let exn_continuationo ppf = function
  | None -> ()
  | Some c -> exn_continuation ppf c

let kinded_variable ppf (v, (kind:okind)) =
  match kind with
  | None ->
    variable ppf v
  | Some _k ->
    failwith "TODO kinded_variable"

let of_kind_value ppf : of_kind_value -> unit = function
  | Symbol s -> symbol ppf s
  | Dynamically_computed v -> variable ppf v
  | Tagged_immediate i -> Format.fprintf ppf "%st" i

let const ppf (c:Fexpr.const) = match c with
  | Naked_immediate i -> Format.fprintf ppf "%su" i
  | Tagged_immediate i -> Format.fprintf ppf "%st" i
  | Naked_float f -> Format.fprintf ppf "%h" f
  | Naked_int32 i -> Format.fprintf ppf "%lil" i
  | Naked_int64 i -> Format.fprintf ppf "%LiL" i
  | Naked_nativeint i -> Format.fprintf ppf "%Li" i

let simple ppf : simple -> unit = function
  | Symbol s -> symbol ppf s
  | Var v -> variable ppf v
  | Const c -> const ppf c

let name ppf : name -> unit = function
  | Symbol s -> symbol ppf s
  | Var v -> variable ppf v

let static_part ppf : static_part -> _ = function
  | Block (tag, _mutability, elts) ->
    Format.fprintf ppf "Block %i (%a)"
      tag
      (pp_space_list of_kind_value) elts


let static_structure ppf (s, (kind:okind), sp) =
  match kind with
  | None ->
    Format.fprintf ppf "@[<2>%a =@ %a@]"
      symbol s
      static_part sp
  | Some _k ->
    failwith "Todo kind static_structure"

let invalid ppf = function
  | Halt_and_catch_fire ->
    Format.fprintf ppf "HCF"
  | Treat_as_unreachable ->
    Format.fprintf ppf "Unreachable"

let infix_binop ppf b =
  let s =
    match b with
    | Plus -> "+"
    | Plusdot -> "+."
    | Minus -> "-"
    | Minusdot -> "-."
  in
  Format.pp_print_string ppf s

let binop ppf binop a b =
  match binop with
  | Block_load (Block Value, Immutable) ->
    Format.fprintf ppf "%a.(%a)"
      simple a simple b
  | Block_load _ ->
    failwith "TODO Block_load"

let unop ppf u =
  let s =
    match u with
    | Opaque_identity -> "Opaque"
  in
  Format.pp_print_string ppf s

let prim ppf = function
  | Unop (u, a) ->
    Format.fprintf ppf "%a %a"
      unop u
      simple a
  | Infix_binop (b, a1, a2) ->
    Format.fprintf ppf "%a %a %a"
      simple a1
      infix_binop b
      simple a2
  | Binop (b, a1, a2) ->
    binop ppf b a1 a2
    (* Format.fprintf ppf "%a %a %a"
     *   binop b a b
     *   simple a1
     *   simple a2 *)
  | Block (tag, Immutable, elts) ->
    Format.fprintf ppf "Block %i (%a)"
      tag
      (pp_space_list simple) elts
  | Block (tag, Immutable_unique, elts) ->
    Format.fprintf ppf "Block_unique %i (%a)"
      tag
      (pp_space_list simple) elts
  | Block (_, Mutable, _) ->
      failwith "TODO mutable block"

let named ppf = function
  | Simple s ->
    simple ppf s
  | Prim p ->
    prim ppf p
  | Read_mutable v ->
    Format.fprintf ppf "!%a" variable v
  | Assign { being_assigned; new_value } ->
    Format.fprintf ppf "%a@ :=@ %a" variable being_assigned simple new_value

let type_ _ppf (():flambda_type) =
  ()

let parameter ppf { param; ty } =
  variable ppf param;
  type_ ppf ty

let typed_parameters ppf = function
  | [] -> ()
  | args ->
    Format.fprintf ppf "@ (@[<hov>%a@])"
      (pp_space_list parameter) args

let closed_elts ppf = function
  | [] -> ()
  | params ->
    Format.fprintf ppf "@ [@[<hov>%a@]]"
      (pp_space_list variable) params

let switch_case ppf (v, c) =
  Format.fprintf ppf "@ %i -> %a"
    v
    continuation c

let switch_sort ppf = function
  | Int -> ()
  | Is_int -> Format.fprintf ppf " is_int"
  | Tag { tags_to_sizes } ->
    let s ppf (tag, size) =
      Format.fprintf ppf "%i:%i" tag size
    in
    Format.fprintf ppf " tag[%a]"
      (pp_comma_list s)
      tags_to_sizes

let simple_args ppf = function
  | [] -> ()
  | args ->
    Format.fprintf ppf "@,@[(@[<hov>%a@])@]@ "
      (pp_space_list simple) args

let return_arity _ppf = function
  | None -> ()
  | Some _ ->
    failwith "TODO"

let rec expr ppf = function
  | Invalid inv ->
    invalid ppf inv
  | Apply_cont (cont, None, []) ->
    Format.fprintf ppf "cont %a" continuation cont
  | Apply_cont (cont, None, args) ->
    Format.fprintf ppf "cont %a (@[<hov>%a@])"
      continuation cont
      (pp_space_list simple) args
  | Let { var = None; kind = None; defining_expr; body } ->
    Format.fprintf ppf "@[<hov>@[<2>%a@];@]@,%a"
      named defining_expr
      expr body
  | Let { var; kind = None; defining_expr; body } ->
    Format.fprintf ppf "@[<hov>@[<2>let %a =@ %a@]@ in@]@ %a"
      variable_opt var
      named defining_expr
      expr body
  | Let_mutable { var; kind = None; initial_value; body } ->
    Format.fprintf ppf "@[<hov>@[<2>let mut %a =@ %a@]@ in@]@ %a"
      variable var
      simple initial_value
      expr body
  | Let_cont { recursive = recu; body;
               handlers =
                 { name; params; stub; is_exn_handler; handler } :: rem_cont;
             } ->
    Format.fprintf ppf "@[<v>@[<v 2>@[<hov 2>letk%a%a%a %a%a@] {@ %a@]@,}%a@ %a@]"
      recursive recu
      is_exn is_exn_handler
      is_stub stub
      continuation name
      typed_parameters params
      expr handler
      andk rem_cont
      expr body

  | Switch { scrutinee; sort; cases } ->
    Format.fprintf ppf "@[<v>@[<v 2>switch%a %a {%a@]@ }@]"
      switch_sort sort
      simple scrutinee
      (pp_semi_list switch_case) cases
      (* (fun ppf () -> if cases <> [] then Format.pp_print_cut ppf ()) () *)

  | Apply {
      call_kind = C_call {
          alloc = true;
          return_arity = None;
        };
      continuation = ret;
      exn_continuation = ek;
      args;
      func = Symbol s } ->
    Format.fprintf ppf "@[<hov 2>ccall@,[%a]%a-> %a %a@]"
      csymbol s
      simple_args args
      continuation ret
      exn_continuation ek

  | Apply {
      call_kind = Function Indirect_unknown_arity;
      continuation = ret;
      exn_continuation = ek;
      args;
      func } ->
    Format.fprintf ppf "@[<hov 2>apply@ %a%a-> %a %a@]"
      name func
      simple_args args
      continuation ret
      exn_continuation ek

  | Let_closure { closures = closure :: rem_closures; body } ->
    let { name; params; closure_vars; ret_cont; exn_cont; ret_arity; expr = e } = closure in
    Format.fprintf ppf "@[<v>@[<v 2>@[<hov 2>closure %a%a%a -> %a%a%a@] {@ %a@]@,}%a@ %a@]"
      variable name
      typed_parameters params
      closed_elts closure_vars

      continuation ret_cont
      exn_continuationo exn_cont
      return_arity ret_arity
      expr e

      andclosure rem_closures

      expr body
  | _ ->
    failwith "TODO"

and andclosure ppf l =
  let closure { name; params; closure_vars; ret_cont; exn_cont; ret_arity; expr = e } =
    Format.fprintf ppf "@[<v 2>@[<hov 2>@ and %a%a%a -> %a%a%a@] {@ %a@]@,}"
      variable name
      typed_parameters params
      closed_elts closure_vars

      continuation ret_cont
      exn_continuationo exn_cont
      return_arity ret_arity
      expr e
  in
  List.iter closure l

and andk ppf l =
  let cont { name; params; stub; is_exn_handler; handler } =
    Format.fprintf ppf " @[<v 2>@[<hov 2>and%a%a %a%a@] {@ %a@]@,}"
      is_exn is_exn_handler
      is_stub stub
      continuation name
      typed_parameters params
      expr handler
  in
  List.iter cont l

let args ppf = function
  | [] -> ()
  | args ->
    Format.fprintf ppf "@ (@[<hov>%a@])"
      (pp_space_list kinded_variable) args

let program_body_elt ppf = function
  | Root s ->
    Format.fprintf ppf "@[root@ %a@]" symbol s
  | Define_symbol (recu, { computation = None; static_structure = s }) ->
    Format.fprintf ppf "@[<v 2>@[<h>def%a@]@ %a@]"
      recursive recu
      (pp_space_list static_structure) s
  | Define_symbol (Nonrecursive, { computation = Some computation; static_structure = [] }) ->
    Format.fprintf ppf "@[<v 2>@[<hov 2>effect@ %a%a%a@]@ %a@]@]"
      continuation computation.return_cont
      exn_continuationo computation.exception_cont
      args computation.computed_values
      expr computation.expr
  | Define_symbol (recu, { computation = Some computation; static_structure = s }) ->
    Format.fprintf ppf "@[<v 2>@[<h>def%a%a@]@ @[<v>@[<v 2>@[<hov 2>letk %a%a@]@ {@[<hov>@ %a@ @]}@]@ %a@]@]"
      recursive recu
      exn_continuationo computation.exception_cont
      continuation computation.return_cont
      args computation.computed_values
      (pp_space_list static_structure) s
      expr computation.expr

  | Let_code { name; params; ret_cont; exn_cont; ret_arity; expr = e } ->
    Format.fprintf ppf "@[<v 2>@[<hov 2>let code %a%a -> %a%a%a =@]@ %a@]"
      symbol name
      typed_parameters params
      continuation ret_cont
      exn_continuationo exn_cont
      return_arity ret_arity
      expr e

let program ppf elts =
  List.iter (fun elt -> Format.fprintf ppf "%a@.@." program_body_elt elt) elts
