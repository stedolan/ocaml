val funcdecl : Mach.fundecl -> Mach.fundecl
val allocates_unconditionally : Mach.instruction -> bool
val is_leaf_func_without_loops : Mach.instruction -> bool
val is_ignored_function : string -> bool



val instrument_fundecl : future_funcnames:Set.Make(String).t -> Mach.fundecl -> Mach.fundecl
