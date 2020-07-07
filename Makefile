#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The main Makefile

ROOTDIR = .

include Makefile.config
include Makefile.common

.PHONY: defaultentry
ifeq "$(NATIVE_COMPILER)" "true"
defaultentry: world.opt
else
defaultentry: world
endif

MKDIR=mkdir -p
ifeq "$(UNIX_OR_WIN32)" "win32"
LN = cp
else
LN = ln -sf
endif

include stdlib/StdlibModules

CAMLC=$(BOOT_OCAMLC) -g -nostdlib -I boot -use-prims runtime/primitives
CAMLOPT=$(CAMLRUN) ./ocamlopt -g -nostdlib -I stdlib -I otherlibs/dynlink
ARCHES=amd64 i386 arm arm64 power s390x
INCLUDES=-I utils -I parsing -I typing -I bytecomp -I file_formats \
        -I lambda -I middle_end -I middle_end/closure \
        -I middle_end/flambda/compilenv_deps \
        -I middle_end/flambda \
        -I middle_end/flambda/basic \
        -I middle_end/flambda/cmx \
        -I middle_end/flambda/from_lambda \
        -I middle_end/flambda/inlining \
        -I middle_end/flambda/lifting \
        -I middle_end/flambda/naming \
        -I middle_end/flambda/parser \
        -I middle_end/flambda/simplify \
        -I middle_end/flambda/simplify/basic \
        -I middle_end/flambda/simplify/env \
        -I middle_end/flambda/simplify/typing_helpers \
        -I middle_end/flambda/terms \
        -I middle_end/flambda/to_cmm \
        -I middle_end/flambda/types \
        -I middle_end/flambda/types/basic \
        -I middle_end/flambda/types/env \
        -I middle_end/flambda/types/kinds \
        -I middle_end/flambda/types/structures \
        -I middle_end/flambda/types/type_of_kind \
        -I middle_end/flambda/types/type_of_kind/boilerplate \
        -I middle_end/flambda/unboxing \
        -I asmcomp -I asmcomp/debug \
        -I driver -I toplevel

COMPFLAGS=-strict-sequence -principal -absname -w +a-4-9-40-41-42-44-45-48-66 \
	  -warn-error A \
          -bin-annot -safe-string -strict-formats $(INCLUDES)
ifeq "$(FUNCTION_SECTIONS)" "true"
OPTCOMPFLAGS= -function-sections
else
OPTCOMPFLAGS=
endif
LINKFLAGS=

ifeq "$(strip $(NATDYNLINKOPTS))" ""
OCAML_NATDYNLINKOPTS=
else
OCAML_NATDYNLINKOPTS = -ccopt "$(NATDYNLINKOPTS)"
endif

YACCFLAGS=-v --strict
CAMLLEX=$(CAMLRUN) boot/ocamllex
CAMLDEP=$(CAMLRUN) boot/ocamlc -depend
DEPFLAGS=-slash
DEPINCLUDES=$(INCLUDES)

OCAMLDOC_OPT=$(WITH_OCAMLDOC:=.opt)

UTILS=utils/config.cmo utils/build_path_prefix_map.cmo utils/misc.cmo \
	utils/identifiable.cmo utils/numbers.cmo utils/arg_helper.cmo \
	utils/clflags.cmo utils/profile.cmo utils/load_path.cmo \
	utils/terminfo.cmo utils/ccomp.cmo utils/warnings.cmo \
	utils/consistbl.cmo utils/strongly_connected_components.cmo \
	utils/one_bit_fewer.cmo \
	utils/targetint.cmo utils/int_replace_polymorphic_compare.cmo \
	utils/domainstate.cmo utils/printing_cache.cmo \
	utils/lmap.cmo

PARSING=parsing/location.cmo parsing/longident.cmo \
  parsing/docstrings.cmo parsing/syntaxerr.cmo \
  parsing/ast_helper.cmo \
  parsing/pprintast.cmo \
  parsing/camlinternalMenhirLib.cmo parsing/parser.cmo \
  parsing/lexer.cmo parsing/parse.cmo parsing/printast.cmo \
  parsing/ast_mapper.cmo parsing/ast_iterator.cmo parsing/attr_helper.cmo \
  parsing/builtin_attributes.cmo parsing/ast_invariants.cmo parsing/depend.cmo

TYPING=typing/ident.cmo typing/path.cmo \
  typing/primitive.cmo typing/type_immediacy.cmo typing/types.cmo \
  typing/btype.cmo typing/oprint.cmo \
  typing/subst.cmo typing/predef.cmo \
  typing/datarepr.cmo file_formats/cmi_format.cmo \
  typing/persistent_env.cmo typing/env.cmo \
  typing/typedtree.cmo typing/printtyped.cmo typing/ctype.cmo \
  typing/printtyp.cmo typing/includeclass.cmo \
  typing/mtype.cmo typing/envaux.cmo typing/includecore.cmo \
  typing/tast_iterator.cmo typing/tast_mapper.cmo \
  file_formats/cmt_format.cmo typing/untypeast.cmo \
  typing/includemod.cmo typing/typetexp.cmo typing/printpat.cmo \
  typing/parmatch.cmo typing/stypes.cmo \
  typing/typedecl_properties.cmo typing/typedecl_variance.cmo \
  typing/typedecl_unboxed.cmo typing/typedecl_immediacy.cmo \
  typing/typedecl.cmo typing/typeopt.cmo \
  typing/rec_check.cmo typing/typecore.cmo typing/typeclass.cmo \
  typing/typemod.cmo

LAMBDA=lambda/tag.cmo lambda/lambda.cmo \
  lambda/debuginfo.cmo lambda/printlambda.cmo \
  lambda/switch.cmo lambda/matching.cmo \
  lambda/translobj.cmo lambda/translattribute.cmo \
  lambda/translprim.cmo lambda/translcore.cmo \
  lambda/dissect_letrec.cmo \
  lambda/translclass.cmo lambda/translmod.cmo \
  lambda/simplif.cmo lambda/runtimedef.cmo

COMP=\
  bytecomp/meta.cmo bytecomp/opcodes.cmo \
  bytecomp/bytesections.cmo bytecomp/dll.cmo \
  bytecomp/symtable.cmo \
  driver/pparse.cmo driver/compenv.cmo \
  driver/main_args.cmo driver/compmisc.cmo \
  driver/makedepend.cmo \
  driver/compile_common.cmo

COMMON=$(UTILS) $(PARSING) $(TYPING) $(LAMBDA) $(COMP)

BYTECOMP=bytecomp/instruct.cmo bytecomp/bytegen.cmo \
  bytecomp/printinstr.cmo bytecomp/emitcode.cmo \
  bytecomp/bytelink.cmo bytecomp/bytelibrarian.cmo bytecomp/bytepackager.cmo \
  driver/errors.cmo driver/compile.cmo

ARCH_SPECIFIC =\
  asmcomp/arch.ml asmcomp/proc.ml asmcomp/CSE.ml asmcomp/selection.ml \
  asmcomp/scheduling.ml asmcomp/reload.ml

INTEL_ASM=\
  asmcomp/x86_proc.cmo \
  asmcomp/x86_dsl.cmo \
  asmcomp/x86_gas.cmo \
  asmcomp/x86_masm.cmo

ARCH_SPECIFIC_ASMCOMP=
ifeq ($(ARCH),i386)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif
ifeq ($(ARCH),amd64)
ARCH_SPECIFIC_ASMCOMP=$(INTEL_ASM)
endif

ASMCOMP_FLAMBDA=\
  middle_end/flambda/to_cmm/un_cps_helper.cmo \
  middle_end/flambda/to_cmm/un_cps_result.cmo \
  middle_end/flambda/to_cmm/un_cps_closure.cmo \
  middle_end/flambda/to_cmm/un_cps_env.cmo \
  middle_end/flambda/to_cmm/un_cps_static.cmo \
  middle_end/flambda/to_cmm/un_cps.cmo

ASMCOMP=\
  $(ARCH_SPECIFIC_ASMCOMP) \
  asmcomp/arch.cmo \
  asmcomp/cmm.cmo asmcomp/printcmm.cmo \
  asmcomp/reg.cmo asmcomp/debug/reg_with_debug_info.cmo \
  asmcomp/debug/reg_availability_set.cmo \
  asmcomp/mach.cmo asmcomp/proc.cmo \
  asmcomp/afl_instrument.cmo \
  asmcomp/strmatch.cmo \
  asmcomp/cmmgen_state.cmo \
  asmcomp/cmm_helpers.cmo \
  asmcomp/cmmgen.cmo \
  $(ASMCOMP_FLAMBDA) \
  asmcomp/interval.cmo \
  asmcomp/printmach.cmo asmcomp/selectgen.cmo \
  asmcomp/spacetime_profiling.cmo asmcomp/selection.cmo \
  asmcomp/comballoc.cmo \
  asmcomp/CSEgen.cmo asmcomp/CSE.cmo \
  asmcomp/liveness.cmo \
  asmcomp/spill.cmo asmcomp/split.cmo \
  asmcomp/interf.cmo asmcomp/coloring.cmo \
  asmcomp/linscan.cmo \
  asmcomp/reloadgen.cmo asmcomp/reload.cmo \
  asmcomp/deadcode.cmo \
  asmcomp/linear.cmo asmcomp/printlinear.cmo asmcomp/linearize.cmo \
  asmcomp/debug/available_regs.cmo \
  asmcomp/debug/compute_ranges_intf.cmo \
  asmcomp/debug/compute_ranges.cmo \
  asmcomp/schedgen.cmo asmcomp/scheduling.cmo \
  asmcomp/branch_relaxation_intf.cmo \
  asmcomp/branch_relaxation.cmo \
  asmcomp/emitaux.cmo asmcomp/emit.cmo asmcomp/asmgen.cmo \
  asmcomp/asmlink.cmo asmcomp/asmlibrarian.cmo asmcomp/asmpackager.cmo \
  driver/opterrors.cmo driver/optcompile.cmo

# Files under middle_end/ are not to reference files under asmcomp/.
# This ensures that the middle end can be linked (e.g. for objinfo) even when
# the native code compiler is not present for some particular target.

MIDDLE_END_CLOSURE=\
  middle_end/closure/closure.cmo \
  middle_end/closure/closure_middle_end.cmo

MIDDLE_END_FLAMBDA_COMPILENV_DEPS=\
  middle_end/flambda/compilenv_deps/table_by_int_id.cmo \
  middle_end/flambda/compilenv_deps/patricia_tree.cmo \
  middle_end/flambda/compilenv_deps/linkage_name.cmo \
  middle_end/flambda/compilenv_deps/target_imm.cmo \
  middle_end/flambda/compilenv_deps/flambda_colours.cmo \
  middle_end/flambda/compilenv_deps/compilation_unit.cmo \
  middle_end/flambda/compilenv_deps/rec_info.cmo \
  middle_end/flambda/compilenv_deps/reg_width_things.cmo \
  middle_end/flambda/compilenv_deps/symbol.cmo \
  middle_end/flambda/compilenv_deps/variable.cmo \
  middle_end/flambda/compilenv_deps/flambda_features.cmo

MIDDLE_END_FLAMBDA_BASIC=\
  middle_end/flambda/types/kinds/flambda_kind.cmo \
  middle_end/flambda/basic/reg_width_const.cmo \
  middle_end/flambda/basic/continuation.cmo \
  middle_end/flambda/basic/name.cmo \
  middle_end/flambda/basic/var_within_closure.cmo \
  middle_end/flambda/basic/id_types.cmo \
  middle_end/flambda/basic/code_id.cmo \
  middle_end/flambda/basic/code_id_or_symbol.cmo \
  middle_end/flambda/basic/mutability.cmo \
  middle_end/flambda/naming/name_mode.cmo \
  middle_end/flambda/naming/permutation.cmo \
  middle_end/flambda/naming/name_permutation.cmo \
  middle_end/flambda/naming/name_occurrences.cmo \
  middle_end/flambda/basic/or_variable.cmo \
  middle_end/flambda/basic/simple.cmo \
  middle_end/flambda/basic/closure_id.cmo \
  middle_end/flambda/cmx/ids_for_export.cmo \
  middle_end/flambda/cmx/contains_ids.cmo \
  middle_end/flambda/basic/kinded_parameter.cmo \
  middle_end/flambda/basic/invariant_env.cmo \
  middle_end/flambda/basic/expr_std.cmo \
  middle_end/flambda/basic/trap_action.cmo \
  middle_end/flambda/basic/exn_continuation.cmo \
  middle_end/flambda/basic/inline_attribute.cmo \
  middle_end/flambda/basic/coeffects.cmo \
  middle_end/flambda/basic/effects.cmo \
  middle_end/flambda/basic/export_id.cmo \
  middle_end/flambda/types/basic/or_unknown.cmo \
  middle_end/flambda/terms/flambda_primitive.cmo \
  middle_end/flambda/basic/recursive.cmo \
  middle_end/flambda/basic/scope.cmo \
  middle_end/flambda/basic/invalid_term_semantics.cmo \
  middle_end/flambda/basic/set_of_closures_origin.cmo \
  middle_end/flambda/basic/num_continuation_uses.cmo \
  middle_end/flambda/basic/function_decl_intf.cmo \
  middle_end/flambda/basic/effects_and_coeffects.cmo \
  middle_end/flambda/basic/continuation_counts.cmo \
  middle_end/flambda/basic/closure_origin.cmo \
  middle_end/flambda/basic/apply_cont_rewrite_id.cmo \
  middle_end/flambda/basic/continuation_extra_params_and_args.cmo

MIDDLE_END_FLAMBDA_NAMING=\
  middle_end/flambda/naming/contains_names.cmo \
  middle_end/flambda/naming/with_delayed_permutation.cmo \
  middle_end/flambda/naming/var_in_binding_pos.cmo \
  middle_end/flambda/naming/bindable.cmo \
  middle_end/flambda/naming/bindable_let_bound.cmo \
  middle_end/flambda/naming/bindable_continuation.cmo \
  middle_end/flambda/naming/bindable_exn_continuation.cmo \
  middle_end/flambda/naming/bindable_variable_in_types.cmo \
  middle_end/flambda/naming/name_abstraction.cmo \
  middle_end/flambda/naming/name_in_binding_pos.cmo \
  middle_end/flambda/naming/bindable_variable_in_terms.cmo

MIDDLE_END_FLAMBDA_TYPES=\
  middle_end/flambda/types/basic/var_within_closure_set.cmo \
  middle_end/flambda/types/kinds/flambda_arity.cmo \
  middle_end/flambda/types/env/binding_time.cmo \
  middle_end/flambda/types/env/aliases.cmo \
  middle_end/flambda/types/basic/meet_or_join_op.cmo \
  middle_end/flambda/types/basic/or_bottom.cmo \
  middle_end/flambda/types/basic/string_info.cmo \
  middle_end/flambda/types/structures/lattice_ops_intf.cmo \
  middle_end/flambda/types/basic/or_bottom_or_absorbing.cmo \
  middle_end/flambda/types/basic/or_unknown_or_bottom.cmo \
  middle_end/flambda/types/structures/code_age_relation.cmo \
  middle_end/flambda/types/structures/type_structure_intf.cmo \
  middle_end/flambda/types/structures/product_intf.cmo \
  middle_end/flambda/types/structures/row_like_maps_to_intf.cmo \
  middle_end/flambda/types/structures/set_of_closures_contents.cmo \
  middle_end/flambda/types/basic/tag_and_size.cmo \
  middle_end/flambda/types/basic/tag_or_unknown_and_size.cmo \
  middle_end/flambda/types/type_descr_intf.cmo \
  middle_end/flambda/types/type_head_intf.cmo \
  middle_end/flambda/types/flambda_type.cmo \
  middle_end/flambda/types/basic/unit.cmo \
  middle_end/flambda/types/basic/or_absorbing.cmo \
  middle_end/flambda/types/basic/closure_id_set.cmo \
  middle_end/flambda/types/basic/closure_id_or_unknown_and_var_within_closure_set.cmo \
  middle_end/flambda/types/basic/closure_id_and_var_within_closure_set.cmo

MIDDLE_END_FLAMBDA_TERMS=\
  middle_end/flambda/terms/apply_cont_expr.cmo \
  middle_end/flambda/terms/call_kind.cmo \
  middle_end/flambda/terms/apply_expr.cmo \
  middle_end/flambda/terms/switch_expr.cmo \
  middle_end/flambda/terms/function_declaration.cmo \
  middle_end/flambda/terms/function_declarations.cmo \
  middle_end/flambda/terms/set_of_closures.cmo \
  middle_end/flambda/terms/flambda.cmo \
  middle_end/flambda/types/flambda_type.cmo \
  middle_end/flambda/terms/flambda_unit.cmo

MIDDLE_END_FLAMBDA_CMX=\
  middle_end/flambda/cmx/exported_offsets.cmo \
  middle_end/flambda/cmx/exported_code.cmo \
  middle_end/flambda/cmx/flambda_cmx_format.cmo

MIDDLE_END_FLAMBDA_SIMPLIFY=\
  middle_end/flambda/simplify/env/continuation_use_kind.cmo \
  middle_end/flambda/simplify/basic/apply_cont_rewrite.cmo \
  middle_end/flambda/simplify/env/continuation_env_and_param_types.cmo \
  middle_end/flambda/simplify/basic/continuation_in_env.cmo \
  middle_end/flambda/simplify/basic/reachable.cmo \
  middle_end/flambda/simplify/env/simplify_env_and_result_intf.cmo \
  middle_end/flambda/simplify/env/simplify_env_and_result.cmo \
  middle_end/flambda/simplify/typing_helpers/one_continuation_use.cmo \
  middle_end/flambda/simplify/typing_helpers/continuation_uses.cmo \
  middle_end/flambda/simplify/env/continuation_uses_env.cmo \
  middle_end/flambda/cmx/flambda_cmx.cmo \
  middle_end/flambda/simplify/env/continuation_uses_env_intf.cmo \
  middle_end/flambda/simplify/env/downwards_acc.cmo \
  middle_end/flambda/simplify/env/upwards_acc.cmo \
  middle_end/flambda/inlining/inlining_cost.cmo \
  middle_end/flambda/inlining/inlining_decision.cmo \
  middle_end/flambda/inlining/inlining_transforms.cmo \
  middle_end/flambda/simplify/simplify_simple.cmo \
  middle_end/flambda/simplify/simplify_import.cmo \
  middle_end/flambda/unboxing/unbox_continuation_params.cmo \
  middle_end/flambda/lifting/sort_lifted_constants.cmo \
  middle_end/flambda/lifting/reification.cmo \
  middle_end/flambda/lifting/lift_inconstants.cmo \
  middle_end/flambda/simplify/basic/reachable.cmo \
  middle_end/flambda/simplify/simplify_common.cmo \
  middle_end/flambda/simplify/simplify_variadic_primitive.cmo \
  middle_end/flambda/simplify/typing_helpers/number_adjuncts.cmo \
  middle_end/flambda/simplify/simplify_unary_primitive.cmo \
  middle_end/flambda/simplify/simplify_ternary_primitive.cmo \
  middle_end/flambda/simplify/simplify_binary_primitive.cmo \
  middle_end/flambda/simplify/simplify_primitive.cmo \
  middle_end/flambda/simplify/typing_helpers/continuation_handler_like_intf.cmo \
  middle_end/flambda/simplify/simplify.cmo

MIDDLE_END_FLAMBDA_FROM_LAMBDA=\
  middle_end/flambda/from_lambda/lambda_conversions.cmo \
  middle_end/flambda/from_lambda/prepare_lambda.cmo \
  middle_end/flambda/from_lambda/ilambda.cmo \
  middle_end/flambda/from_lambda/closure_conversion_aux.cmo \
  middle_end/flambda/from_lambda/delayed_handlers.cmo \
  middle_end/flambda/from_lambda/lambda_to_flambda_primitives_helpers.cmo \
  middle_end/flambda/from_lambda/lambda_to_flambda_primitives.cmo \
  middle_end/flambda/from_lambda/closure_conversion.cmo \
  middle_end/flambda/from_lambda/cps_conversion.cmo \
  middle_end/flambda/from_lambda/eliminate_mutable_vars.cmo

MIDDLE_END_FLAMBDA_PARSER=\
  middle_end/flambda/parser/fexpr.cmo \
  middle_end/flambda/parser/print_fexpr.cmo \
  middle_end/flambda/parser/fexpr_to_flambda.cmo \
  middle_end/flambda/parser/fexpr_to_ilambda.cmo \
  middle_end/flambda/parser/flambda_parser.cmo \
  middle_end/flambda/parser/flambda_lex.cmo \
  middle_end/flambda/parser/parse_ilambda.cmo

MIDDLE_END_FLAMBDA=\
  $(MIDDLE_END_FLAMBDA_BASIC) \
  $(MIDDLE_END_FLAMBDA_NAMING) \
  $(MIDDLE_END_FLAMBDA_TYPES) \
  $(MIDDLE_END_FLAMBDA_TERMS) \
  $(MIDDLE_END_FLAMBDA_CMX) \
  $(MIDDLE_END_FLAMBDA_SIMPLIFY) \
  $(MIDDLE_END_FLAMBDA_FROM_LAMBDA) \
  $(MIDDLE_END_FLAMBDA_PARSER) \
  middle_end/flambda/flambda_middle_end.cmo

MIDDLE_END=\
  middle_end/internal_variable_names.cmo \
  middle_end/backend_var.cmo \
  middle_end/clambda_primitives.cmo \
  middle_end/printclambda_primitives.cmo \
  middle_end/clambda.cmo \
  middle_end/printclambda.cmo \
  middle_end/semantics_of_primitives.cmo \
  middle_end/convert_primitives.cmo \
  $(MIDDLE_END_FLAMBDA_COMPILENV_DEPS) \
  middle_end/compilenv.cmo \
  $(MIDDLE_END_CLOSURE) \
  $(MIDDLE_END_FLAMBDA)

OPTCOMP=$(MIDDLE_END) $(ASMCOMP)

TOPLEVEL=toplevel/genprintval.cmo toplevel/toploop.cmo \
  toplevel/trace.cmo toplevel/topdirs.cmo toplevel/topmain.cmo

OPTTOPLEVEL=toplevel/genprintval.cmo toplevel/opttoploop.cmo \
  toplevel/opttopdirs.cmo toplevel/opttopmain.cmo
BYTESTART=driver/main.cmo

OPTSTART=driver/optmain.cmo

ILAMBDASTART=driver/ilambdac.cmo

TOPLEVELSTART=toplevel/topstart.cmo

OPTTOPLEVELSTART=toplevel/opttopstart.cmo

PERVASIVES=$(STDLIB_MODULES) outcometree topdirs toploop

LIBFILES=stdlib.cma std_exit.cmo *.cmi camlheader

COMPLIBDIR=$(LIBDIR)/compiler-libs

TOPINCLUDES=$(addprefix -I otherlibs/,$(filter-out %threads,$(OTHERLIBRARIES)))
RUNTOP=./runtime/ocamlrun ./ocaml \
  -nostdlib -I stdlib \
  -noinit $(TOPFLAGS) $(TOPINCLUDES)
NATRUNTOP=./ocamlnat$(EXE) \
  -nostdlib -I stdlib \
  -noinit $(TOPFLAGS) $(TOPINCLUDES)
ifeq "$(UNIX_OR_WIN32)" "unix"
EXTRAPATH=
else
EXTRAPATH = PATH="otherlibs/win32unix:$(PATH)"
endif

BOOT_FLEXLINK_CMD=

ifeq "$(UNIX_OR_WIN32)" "win32"
FLEXDLL_SUBMODULE_PRESENT := $(wildcard flexdll/Makefile)
ifeq "$(FLEXDLL_SUBMODULE_PRESENT)" ""
  BOOT_FLEXLINK_CMD =
else
  BOOT_FLEXLINK_CMD = FLEXLINK_CMD="../boot/ocamlrun ../flexdll/flexlink.exe"
endif
else
endif

# The configuration file

utils/config.ml: utils/config.mlp Makefile.config utils/Makefile
	$(MAKE) -C utils config.ml

.PHONY: reconfigure
reconfigure:
	ac_read_git_config=true ./configure $(CONFIGURE_ARGS)

utils/domainstate.ml: utils/domainstate.ml.c runtime/caml/domain_state.tbl
	$(CPP) -I runtime/caml $< > $@

utils/domainstate.mli: utils/domainstate.mli.c runtime/caml/domain_state.tbl
	$(CPP) -I runtime/caml $< > $@

.PHONY: partialclean
partialclean::
	rm -f utils/config.ml utils/domainstate.ml utils/domainstate.mli

.PHONY: beforedepend
beforedepend:: utils/config.ml utils/domainstate.ml utils/domainstate.mli

# Start up the system from the distribution compiler
.PHONY: coldstart
coldstart:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) \
	  CAMLC='$$(BOOT_OCAMLC) -use-prims ../runtime/primitives' all
	cd stdlib; cp $(LIBFILES) ../boot
	cd boot; $(LN) ../runtime/libcamlrun.$(A) .

# Recompile the core system using the bootstrap compiler
.PHONY: coreall
coreall: runtime
	$(MAKE) ocamlc
	$(MAKE) ocamllex ocamltools library

# Build the core system: the minimum needed to make depend and bootstrap
.PHONY: core
core:
	$(MAKE) coldstart
	$(MAKE) coreall

# Check if fixpoint reached
.PHONY: compare
compare:
	@if $(CAMLRUN) tools/cmpbyt boot/ocamlc ocamlc \
         && $(CAMLRUN) tools/cmpbyt boot/ocamllex lex/ocamllex; \
	then echo "Fixpoint reached, bootstrap succeeded."; \
	else \
	  echo "Fixpoint not reached, try one more bootstrapping cycle."; \
	  exit 1; \
	fi

# Promote a compiler

PROMOTE ?= cp

.PHONY: promote-common
promote-common:
	$(PROMOTE) ocamlc boot/ocamlc
	$(PROMOTE) lex/ocamllex boot/ocamllex
	cd stdlib; cp $(LIBFILES) ../boot

# Promote the newly compiled system to the rank of cross compiler
# (Runs on the old runtime, produces code for the new runtime)
.PHONY: promote-cross
promote-cross: promote-common

# Promote the newly compiled system to the rank of bootstrap compiler
# (Runs on the new runtime, produces code for the new runtime)
.PHONY: promote
promote: PROMOTE = $(CAMLRUN) tools/stripdebug
promote: promote-common
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)

# Compile the native-code compiler
.PHONY: opt-core
opt-core: runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt

.PHONY: opt
opt: checknative
	$(MAKE) runtimeopt
	$(MAKE) ocamlopt
	$(MAKE) libraryopt
	$(MAKE) otherlibrariesopt ocamltoolsopt

# Native-code versions of the tools
.PHONY: opt.opt
opt.opt: checknative
	$(MAKE) checkstack
	$(MAKE) runtime
	$(MAKE) core
	$(MAKE) ocaml
	$(MAKE) opt-core
	$(MAKE) ocamlc.opt
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest
	$(MAKE) ocamlopt.opt
	$(MAKE) otherlibrariesopt
	$(MAKE) ocamllex.opt ocamltoolsopt ocamltoolsopt.opt $(OCAMLDOC_OPT) \
	  ocamltest.opt
ifneq "$(WITH_OCAMLDOC)" ""
	$(MAKE) manpages
endif

# Core bootstrapping cycle
.PHONY: coreboot
coreboot:
# Promote the new compiler but keep the old runtime
# This compiler runs on boot/ocamlrun and produces bytecode for
# runtime/ocamlrun
	$(MAKE) promote-cross
# Rebuild ocamlc and ocamllex (run on runtime/ocamlrun)
	$(MAKE) partialclean
	$(MAKE) ocamlc ocamllex ocamltools
# Rebuild the library (using runtime/ocamlrun ./ocamlc)
	$(MAKE) library-cross
# Promote the new compiler and the new runtime
	$(MAKE) CAMLRUN=runtime/ocamlrun promote
# Rebuild the core system
	$(MAKE) partialclean
	$(MAKE) core
# Check if fixpoint reached
	$(MAKE) compare

# Recompile the system using the bootstrap compiler

.PHONY: all
all: coreall
	$(MAKE) ocaml
	$(MAKE) otherlibraries $(WITH_DEBUGGER) $(WITH_OCAMLDOC) ocamltest
ifneq "$(WITH_OCAMLDOC)" ""
	$(MAKE) manpages
endif

# Bootstrap and rebuild the whole system.
# The compilation of ocaml will fail if the runtime has changed.
# Never mind, just do make bootstrap to reach fixpoint again.
.PHONY: bootstrap
bootstrap: coreboot
	$(MAKE) all

# Compile everything the first time

.PHONY: world
world: coldstart
	$(MAKE) all

# Compile also native code compiler and libraries, fast
.PHONY: world.opt
world.opt: checknative
	$(MAKE) coldstart
	$(MAKE) opt.opt

# FlexDLL sources missing error messages
# Different git mechanism displayed depending on whether this source tree came
# from a git clone or a source tarball.

flexdll/Makefile:
	@echo In order to bootstrap FlexDLL, you need to place the sources in
	@echo flexdll.
	@echo This can either be done by downloading a source tarball from
	@echo \  http://alain.frisch.fr/flexdll.html
	@if [ -d .git ]; then \
	  echo or by checking out the flexdll submodule with; \
	  echo \  git submodule update --init; \
	else \
	  echo or by cloning the git repository; \
	  echo \  git clone https://github.com/alainfrisch/flexdll.git; \
	fi
	@false

.PHONY: flexdll
flexdll: flexdll/Makefile flexlink
	$(MAKE) -C flexdll \
	     OCAML_CONFIG_FILE=../Makefile.config \
             MSVC_DETECT=0 CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false support

# Bootstrapping flexlink - leaves a bytecode image of flexlink.exe in flexdll/
.PHONY: flexlink
flexlink: flexdll/Makefile
	$(MAKE) -C runtime BOOTSTRAPPING_FLEXLINK=yes ocamlrun$(EXE)
	cp runtime/ocamlrun$(EXE) boot/ocamlrun$(EXE)
	$(MAKE) -C stdlib COMPILER=../boot/ocamlc \
	                  $(filter-out *.cmi,$(LIBFILES))
	cd stdlib && cp $(LIBFILES) ../boot/
	$(MAKE) -C flexdll MSVC_DETECT=0 OCAML_CONFIG_FILE=../Makefile.config \
	  CHAINS=$(FLEXDLL_CHAIN) NATDYNLINK=false \
	  OCAMLOPT="../boot/ocamlrun ../boot/ocamlc -nostdlib -I ../boot" \
	  flexlink.exe
	$(MAKE) -C runtime clean
	$(MAKE) partialclean

.PHONY: flexlink.opt
flexlink.opt:
	cd flexdll && \
	mv flexlink.exe flexlink && \
	($(MAKE) OCAML_FLEXLINK="../boot/ocamlrun ./flexlink" MSVC_DETECT=0 \
	           OCAML_CONFIG_FILE=../Makefile.config \
	           OCAMLOPT="../ocamlopt.opt -nostdlib -I ../stdlib" \
	           flexlink.exe || \
	 (mv flexlink flexlink.exe && false)) && \
	mv flexlink.exe flexlink.opt && \
	mv flexlink flexlink.exe

INSTALL_COMPLIBDIR=$(DESTDIR)$(COMPLIBDIR)
INSTALL_FLEXDLLDIR=$(INSTALL_LIBDIR)/flexdll

.PHONY: install-flexdll
install-flexdll:
	$(INSTALL_PROG) flexdll/flexlink.exe "$(INSTALL_BINDIR)/flexlink$(EXE)"
ifneq "$(filter-out mingw,$(TOOLCHAIN))" ""
	$(INSTALL_DATA) flexdll/default$(filter-out _i386,_$(ARCH)).manifest \
    "$(INSTALL_BINDIR)/"
endif
	if test -n "$(wildcard flexdll/flexdll_*.$(O))" ; then \
	  $(MKDIR) "$(INSTALL_FLEXDLLDIR)" ; \
	  $(INSTALL_DATA) flexdll/flexdll_*.$(O) "$(INSTALL_FLEXDLLDIR)" ; \
	fi

# Installation
.PHONY: install
install:
	$(MKDIR) "$(INSTALL_BINDIR)"
	$(MKDIR) "$(INSTALL_LIBDIR)"
	$(MKDIR) "$(INSTALL_STUBLIBDIR)"
	$(MKDIR) "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	  VERSION \
	  "$(INSTALL_LIBDIR)"
	$(MAKE) -C runtime install
	$(INSTALL_PROG) ocaml "$(INSTALL_BINDIR)/ocaml$(EXE)"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlc "$(INSTALL_BINDIR)/ocamlc.byte$(EXE)"
endif
	$(MAKE) -C stdlib install
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) lex/ocamllex "$(INSTALL_BINDIR)/ocamllex.byte$(EXE)"
endif
	$(INSTALL_PROG) yacc/ocamlyacc$(EXE) "$(INSTALL_BINDIR)/ocamlyacc$(EXE)"
	$(INSTALL_DATA) \
	   utils/*.cmi \
	   parsing/*.cmi \
	   typing/*.cmi \
	   bytecomp/*.cmi \
	   file_formats/*.cmi \
	   lambda/*.cmi \
	   driver/*.cmi \
	   toplevel/*.cmi \
	   "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.cmt utils/*.cmti utils/*.mli \
	   parsing/*.cmt parsing/*.cmti parsing/*.mli \
	   typing/*.cmt typing/*.cmti typing/*.mli \
	   file_formats/*.cmt file_formats/*.cmti file_formats/*.mli \
	   lambda/*.cmt lambda/*.cmti lambda/*.mli \
	   bytecomp/*.cmt bytecomp/*.cmti bytecomp/*.mli \
	   driver/*.cmt driver/*.cmti driver/*.mli \
	   toplevel/*.cmt toplevel/*.cmti toplevel/*.mli \
	   "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	  compilerlibs/*.cma \
	  "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART) $(TOPLEVELSTART) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_PROG) expunge "$(INSTALL_LIBDIR)/expunge$(EXE)"
	$(INSTALL_DATA) \
	   toplevel/topdirs.cmi \
	   "$(INSTALL_LIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   toplevel/topdirs.cmt toplevel/topdirs.cmti \
           toplevel/topdirs.mli \
	   "$(INSTALL_LIBDIR)"
endif
	$(MAKE) -C tools install
ifeq "$(UNIX_OR_WIN32)" "unix" # Install manual pages only on Unix
	$(MKDIR) "$(INSTALL_MANDIR)/man$(PROGRAMS_MAN_SECTION)"
	-$(MAKE) -C man install
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i install || exit $$?; \
	done
# Transitional: findlib 1.7.3 is confused if leftover num.cm? files remain
# from an previous installation of OCaml before otherlibs/num was removed.
	rm -f "$(INSTALL_LIBDIR)"/num.cm?
# End transitional
ifneq "$(WITH_OCAMLDOC)" ""
	$(MAKE) -C ocamldoc install
endif
	if test -n "$(WITH_DEBUGGER)"; then \
	  $(MAKE) -C debugger install; \
	fi
ifeq "$(UNIX_OR_WIN32)" "win32"
	if test -n "$(FLEXDLL_SUBMODULE_PRESENT)"; then \
	  $(MAKE) install-flexdll; \
	fi
endif
	$(INSTALL_DATA) Makefile.config "$(INSTALL_LIBDIR)/Makefile.config"
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt; then $(MAKE) installopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi
else
	if test -f ocamlopt; then $(MAKE) installopt; fi
endif

# Installation of the native-code compiler
.PHONY: installopt
installopt:
	$(MAKE) -C runtime installopt
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	$(INSTALL_PROG) ocamlopt "$(INSTALL_BINDIR)/ocamlopt.byte$(EXE)"
endif
	$(MAKE) -C stdlib installopt
	$(INSTALL_DATA) \
	    middle_end/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
#	$(INSTALL_DATA) \
#	    middle_end/flambda/*.cmi \
#	    "$(INSTALL_COMPLIBDIR)"
#	$(INSTALL_DATA) \
#	    middle_end/flambda/base_types/*.cmi \
#	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/from_lambda/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/terms/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/debug/*.cmi \
	    "$(INSTALL_COMPLIBDIR)"
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	    middle_end/*.cmt middle_end/*.cmti \
	    middle_end/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/closure/*.cmt middle_end/closure/*.cmti \
	    middle_end/closure/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/*.cmt \
            middle_end/flambda/*.cmti \
	    middle_end/flambda/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/from_lambda/*.cmt \
            middle_end/flambda/from_lambda/*.cmti \
	    middle_end/flambda/from_lambda/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    middle_end/flambda/terms/*.cmt \
            middle_end/flambda/terms/*.cmti \
	    middle_end/flambda/terms/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/*.cmt asmcomp/*.cmti \
	    asmcomp/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	    asmcomp/debug/*.cmt asmcomp/debug/*.cmti \
	    asmcomp/debug/*.mli \
	    "$(INSTALL_COMPLIBDIR)"
endif
	$(INSTALL_DATA) \
	    $(OPTSTART) \
	    "$(INSTALL_COMPLIBDIR)"
ifneq "$(WITH_OCAMLDOC)" ""
	$(MAKE) -C ocamldoc installopt
endif
	for i in $(OTHERLIBRARIES); do \
	  $(MAKE) -C otherlibs/$$i installopt || exit $$?; \
	done
ifeq "$(INSTALL_BYTECODE_PROGRAMS)" "true"
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; else \
	   cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.byte$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.byte$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.byte$(EXE) ocamllex$(EXE); \
	fi
else
	if test -f ocamlopt.opt ; then $(MAKE) installoptopt; fi
endif
	$(MAKE) -C tools installopt
	if test -f ocamlopt.opt -a -f flexdll/flexlink.opt ; then \
	  $(INSTALL_PROG) \
	    flexdll/flexlink.opt "$(INSTALL_BINDIR)/flexlink$(EXE)" ; \
	fi

.PHONY: installoptopt
installoptopt:
	$(INSTALL_PROG) ocamlc.opt "$(INSTALL_BINDIR)/ocamlc.opt$(EXE)"
	$(INSTALL_PROG) ocamlopt.opt "$(INSTALL_BINDIR)/ocamlopt.opt$(EXE)"
	$(INSTALL_PROG) \
	  lex/ocamllex.opt "$(INSTALL_BINDIR)/ocamllex.opt$(EXE)"
	cd "$(INSTALL_BINDIR)"; \
	   $(LN) ocamlc.opt$(EXE) ocamlc$(EXE); \
	   $(LN) ocamlopt.opt$(EXE) ocamlopt$(EXE); \
	   $(LN) ocamllex.opt$(EXE) ocamllex$(EXE)
	$(INSTALL_DATA) \
	   utils/*.cmx parsing/*.cmx typing/*.cmx bytecomp/*.cmx \
	   file_formats/*.cmx \
	   lambda/*.cmx \
	   driver/*.cmx asmcomp/*.cmx middle_end/*.cmx \
           middle_end/closure/*.cmx \
	   asmcomp/debug/*.cmx \
           middle_end/flambda/*.cmx \
	   middle_end/flambda/from_lambda/*.cmx \
	   middle_end/flambda/terms/*.cmx \
          "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   compilerlibs/*.cmxa compilerlibs/*.$(A) \
	   "$(INSTALL_COMPLIBDIR)"
	$(INSTALL_DATA) \
	   $(BYTESTART:.cmo=.cmx) $(BYTESTART:.cmo=.$(O)) \
	   $(OPTSTART:.cmo=.cmx) $(OPTSTART:.cmo=.$(O)) \
	   "$(INSTALL_COMPLIBDIR)"
	if test -f ocamlnat$(EXE) ; then \
	  $(INSTALL_PROG) \
	    ocamlnat$(EXE) "$(INSTALL_BINDIR)/ocamlnat$(EXE)"; \
	  $(INSTALL_DATA) \
	     toplevel/opttopdirs.cmi \
	     "$(INSTALL_LIBDIR)"; \
	  $(INSTALL_DATA) \
	     $(OPTTOPLEVELSTART:.cmo=.cmx) $(OPTTOPLEVELSTART:.cmo=.$(O)) \
	     "$(INSTALL_COMPLIBDIR)"; \
	fi
	cd "$(INSTALL_COMPLIBDIR)" && \
	   $(RANLIB) ocamlcommon.$(A) ocamlbytecomp.$(A) ocamloptcomp.$(A)

# Installation of the *.ml sources of compiler-libs
.PHONY: install-compiler-sources
install-compiler-sources:
ifeq "$(INSTALL_SOURCE_ARTIFACTS)" "true"
	$(INSTALL_DATA) \
	   utils/*.ml parsing/*.ml typing/*.ml bytecomp/*.ml driver/*.ml \
           file_formats/*.ml \
           lambda/*.ml \
	   toplevel/*.ml middle_end/*.ml middle_end/closure/*.ml \
     middle_end/flambda/*.ml middle_end/flambda/base_types/*.ml \
	   asmcomp/*.ml \
	   asmcmp/debug/*.ml \
	   middle_end/flambda/*.ml \
	   middle_end/flambda/from_lambda/*.ml \
	   middle_end/flambda/types/*.ml \
	   "$(INSTALL_COMPLIBDIR)"
endif

# Run all tests

.PHONY: tests
tests:
	$(MAKE) -C testsuite all

# Make clean in the test suite

.PHONY: clean
clean::
	$(MAKE) -C testsuite clean

# Build the manual latex files from the etex source files
# (see manual/README.md)
.PHONY: manual-pregen
manual-pregen: opt.opt
	cd manual; $(MAKE) clean && $(MAKE) pregen-etex

# The clean target
clean:: partialclean

# Shared parts of the system

compilerlibs/ocamlcommon.cma: $(COMMON)
	$(CAMLC) -a -linkall -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlcommon.cma

# The bytecode compiler

compilerlibs/ocamlbytecomp.cma: $(BYTECOMP)
	$(CAMLC) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cma

ocamlc: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma $(BYTESTART)
	$(CAMLC) $(LINKFLAGS) -compat-32 -o $@ $^

partialclean::
	rm -rf ocamlc

# The native-code compiler

compilerlibs/ocamloptcomp.cma: $(OPTCOMP)
	$(CAMLC) -a -o $@ $^

partialclean::
	rm -f compilerlibs/ocamloptcomp.cma

ocamlopt: compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          $(OPTSTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

ilambdac: compilerlibs/ocamlcommon.cma compilerlibs/ocamloptcomp.cma \
          $(ILAMBDASTART)
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt

# The toplevel

compilerlibs/ocamltoplevel.cma: $(TOPLEVEL)
	$(CAMLC) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamltoplevel.cma

ocaml_dependencies := \
  compilerlibs/ocamlcommon.cma \
  compilerlibs/ocamlbytecomp.cma \
  compilerlibs/ocamltoplevel.cma $(TOPLEVELSTART)

.INTERMEDIATE: ocaml.tmp
ocaml.tmp: $(ocaml_dependencies)
	$(CAMLC) $(LINKFLAGS) -linkall -o $@ $^

ocaml: expunge ocaml.tmp
	- $(CAMLRUN) $^ $@ $(PERVASIVES)

partialclean::
	rm -f ocaml

.PHONY: runtop
runtop:
	$(MAKE) coldstart
	$(MAKE) ocamlc
	$(MAKE) otherlibraries
	$(MAKE) ocaml
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(RUNTOP) ||\
	  $(EXTRAPATH) $(RUNTOP)

.PHONY: natruntop
natruntop:
	$(MAKE) core
	$(MAKE) opt
	$(MAKE) ocamlnat
	@rlwrap --help 2>/dev/null && $(EXTRAPATH) rlwrap $(NATRUNTOP) ||\
	  $(EXTRAPATH) $(NATRUNTOP)

# Native dynlink

otherlibs/dynlink/dynlink.cmxa: otherlibs/dynlink/native/dynlink.ml
	$(MAKE) -C otherlibs/dynlink allopt

# The lexer

parsing/lexer.ml: parsing/lexer.mll
	$(CAMLLEX) $(OCAMLLEX_FLAGS) $<

partialclean::
	rm -f parsing/lexer.ml

beforedepend:: parsing/lexer.ml

# Shared parts of the system compiled with the native-code compiler

compilerlibs/ocamlcommon.cmxa: $(COMMON:.cmo=.cmx)
	$(CAMLOPT) -a -linkall -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlcommon.$(A)

# The bytecode compiler compiled with the native-code compiler

compilerlibs/ocamlbytecomp.cmxa: $(BYTECOMP:.cmo=.cmx)
	$(CAMLOPT) -a $(OCAML_NATDYNLINKOPTS) -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlbytecomp.cmxa compilerlibs/ocamlbytecomp.$(A)

ocamlc.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamlbytecomp.cmxa \
            $(BYTESTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^ -cclib "$(BYTECCLIBS)"

partialclean::
	rm -f ocamlc.opt

# The native-code compiler compiled with itself

compilerlibs/ocamloptcomp.cmxa: $(OPTCOMP:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamloptcomp.cmxa compilerlibs/ocamloptcomp.$(A)

ocamlopt.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
              $(OPTSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

ilambdac.opt: compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
              $(ILAMBDASTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f ocamlopt.opt

$(COMMON:.cmo=.cmx) $(BYTECOMP:.cmo=.cmx) $(OPTCOMP:.cmo=.cmx): ocamlopt

# The predefined exceptions and primitives

runtime/primitives:
	$(MAKE) -C runtime primitives

lambda/runtimedef.ml: lambda/generate_runtimedef.sh runtime/caml/fail.h \
    runtime/primitives
	$^ > $@

partialclean::
	rm -f lambda/runtimedef.ml

beforedepend:: lambda/runtimedef.ml

# Choose the right machine-dependent files

asmcomp/arch.ml: asmcomp/$(ARCH)/arch.ml
	cd asmcomp; $(LN) $(ARCH)/arch.ml .

asmcomp/proc.ml: asmcomp/$(ARCH)/proc.ml
	cd asmcomp; $(LN) $(ARCH)/proc.ml .

asmcomp/selection.ml: asmcomp/$(ARCH)/selection.ml
	cd asmcomp; $(LN) $(ARCH)/selection.ml .

asmcomp/CSE.ml: asmcomp/$(ARCH)/CSE.ml
	cd asmcomp; $(LN) $(ARCH)/CSE.ml .

asmcomp/reload.ml: asmcomp/$(ARCH)/reload.ml
	cd asmcomp; $(LN) $(ARCH)/reload.ml .

asmcomp/scheduling.ml: asmcomp/$(ARCH)/scheduling.ml
	cd asmcomp; $(LN) $(ARCH)/scheduling.ml .

# Preprocess the code emitters

asmcomp/emit.ml: asmcomp/$(ARCH)/emit.mlp tools/cvt_emit
	echo \# 1 \"$(ARCH)/emit.mlp\" > $@
	$(CAMLRUN) tools/cvt_emit < $< >> $@ \
	|| { rm -f $@; exit 2; }

partialclean::
	rm -f asmcomp/emit.ml

beforedepend:: asmcomp/emit.ml

tools/cvt_emit: tools/cvt_emit.mll
	$(MAKE) -C tools cvt_emit

# The "expunge" utility

expunge: compilerlibs/ocamlcommon.cma compilerlibs/ocamlbytecomp.cma \
         toplevel/expunge.cmo
	$(CAMLC) $(LINKFLAGS) -o $@ $^

partialclean::
	rm -f expunge

# The runtime system for the bytecode compiler

.PHONY: runtime
runtime: stdlib/libcamlrun.$(A)

.PHONY: makeruntime
makeruntime:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) all
runtime/libcamlrun.$(A): makeruntime ;
stdlib/libcamlrun.$(A): runtime/libcamlrun.$(A)
	cd stdlib; $(LN) ../runtime/libcamlrun.$(A) .
clean::
	$(MAKE) -C runtime clean
	rm -f stdlib/libcamlrun.$(A)

otherlibs_all := bigarray dynlink raw_spacetime_lib \
  str systhreads unix win32unix
subdirs := debugger lex ocamldoc ocamltest runtime stdlib tools \
  $(addprefix otherlibs/, $(otherlibs_all)) \

.PHONY: alldepend
ifeq "$(TOOLCHAIN)" "msvc"
alldepend:
	$(error Dependencies cannot be regenerated using the MSVC ports)
else
alldepend: depend
	for dir in $(subdirs); do \
	  $(MAKE) -C $$dir depend || exit; \
	done
endif

# The runtime system for the native-code compiler

.PHONY: runtimeopt
runtimeopt: stdlib/libasmrun.$(A)

.PHONY: makeruntimeopt
makeruntimeopt:
	$(MAKE) -C runtime $(BOOT_FLEXLINK_CMD) allopt
runtime/libasmrun.$(A): makeruntimeopt ;
stdlib/libasmrun.$(A): runtime/libasmrun.$(A)
	cp $< $@
clean::
	rm -f stdlib/libasmrun.$(A)

# The standard library

.PHONY: library
library: ocamlc
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) all

.PHONY: library-cross
library-cross:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) CAMLRUN=../runtime/ocamlrun all

.PHONY: libraryopt
libraryopt:
	$(MAKE) -C stdlib $(BOOT_FLEXLINK_CMD) allopt

partialclean::
	$(MAKE) -C stdlib clean

# The lexer and parser generators

.PHONY: ocamllex
ocamllex: ocamlyacc
	$(MAKE) -C lex all

.PHONY: ocamllex.opt
ocamllex.opt: ocamlopt
	$(MAKE) -C lex allopt

partialclean::
	$(MAKE) -C lex clean

.PHONY: ocamlyacc
ocamlyacc:
	$(MAKE) -C yacc $(BOOT_FLEXLINK_CMD) all

clean::
	$(MAKE) -C yacc clean

# The Menhir-generated parser

# In order to avoid a build-time dependency on Menhir,
# we store the result of the parser generator (which
# are OCaml source files) and Menhir's runtime libraries
# (that the parser files rely on) in boot/.

# The rules below do not depend on Menhir being available,
# they just build the parser from boot/.

# See Makefile.menhir for the rules to rebuild the parser and update
# boot/, which require Menhir. The targets in Makefile.menhir
# (also included here for convenience) must be used after any
# modification of parser.mly.
include Makefile.menhir

# To avoid module-name conflicts with compiler-lib users that link
# with their code with their own MenhirLib module (possibly with
# a different Menhir version), we rename MenhirLib into
# CamlinternalMenhirlib -- and replace the module occurrences in the
# generated parser.ml.

parsing/camlinternalMenhirLib.ml: boot/menhir/menhirLib.ml
	cp $< $@
parsing/camlinternalMenhirLib.mli: boot/menhir/menhirLib.mli
	echo '[@@@ocaml.warning "-67"]' > $@
	cat $< >> $@

# Copy parsing/parser.ml from boot/

parsing/parser.ml: boot/menhir/parser.ml parsing/parser.mly \
  tools/check-parser-uptodate-or-warn.sh
	@-tools/check-parser-uptodate-or-warn.sh
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@
parsing/parser.mli: boot/menhir/parser.mli
	sed "s/MenhirLib/CamlinternalMenhirLib/g" $< > $@

beforedepend:: parsing/camlinternalMenhirLib.ml \
  parsing/camlinternalMenhirLib.mli \
	parsing/parser.ml parsing/parser.mli

partialclean:: partialclean-menhir

# OCamldoc

.PHONY: ocamldoc
ocamldoc: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C ocamldoc all

.PHONY: ocamldoc.opt
ocamldoc.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamldoc opt.opt

# OCamltest
ocamltest: ocamlc ocamlyacc ocamllex
	$(MAKE) -C ocamltest all

ocamltest.opt: ocamlc.opt ocamlyacc ocamllex
	$(MAKE) -C ocamltest allopt

partialclean::
	$(MAKE) -C ocamltest clean

# Flambda recursive modules black magic

FLAMBDA_TYPE0_DEPS=$(shell sed -e "s|^\(.*\)|middle_end/flambda/types/\1 middle_end/flambda/types/\1i|g" \
  middle_end/flambda/types/rec_modules | tr '\n' ' ')
middle_end/flambda/types/flambda_type.ml: \
  middle_end/flambda/types/template/flambda_type.templ.ml \
  middle_end/flambda/types/rec_modules \
  $(FLAMBDA_TYPE0_DEPS)
	cd middle_end/flambda/types && \
	  ../scripts/assemble_rec_modules.sh template/flambda_type.templ.ml \
	    rec_modules flambda_type.ml

FLAMBDA_DEPS=$(shell sed -e "s|^\(.*\)|middle_end/flambda/terms/\1 middle_end/flambda/terms/\1i|g" \
  middle_end/flambda/terms/rec_modules | tr '\n' ' ')
middle_end/flambda/terms/flambda.ml: \
  middle_end/flambda/terms/template/flambda.templ.ml \
  middle_end/flambda/terms/rec_modules \
  $(FLAMBDA_DEPS)
	cd middle_end/flambda/terms && \
	  ../scripts/assemble_rec_modules.sh template/flambda.templ.ml \
	    rec_modules flambda.ml

SIMPLIFY_DEPS=$(shell sed -e "s|^\(.*\)|middle_end/flambda/simplify/\1 middle_end/flambda/simplify/\1i|g" \
  middle_end/flambda/simplify/rec_modules | tr '\n' ' ')
middle_end/flambda/simplify/simplify.ml: \
  middle_end/flambda/simplify/template/simplify.templ.ml \
  middle_end/flambda/simplify/rec_modules \
  $(SIMPLIFY_DEPS)
	cd middle_end/flambda/simplify && \
	  ../scripts/assemble_rec_modules.sh template/simplify.templ.ml \
	    rec_modules simplify.ml

beforedepend:: \
  middle_end/flambda/types/flambda_type.ml \
  middle_end/flambda/terms/flambda.ml \
  middle_end/flambda/simplify/simplify.ml

clean::
	rm -f middle_end/flambda/types/flambda_type.ml;
	rm -f middle_end/flambda/terms/flambda.ml;
	rm -f middle_end/flambda/simplify/simplify.ml


# Documentation

.PHONY: html_doc
html_doc: ocamldoc
	$(MAKE) -C ocamldoc $@
	@echo "documentation is in ./ocamldoc/stdlib_html/"

.PHONY: manpages
manpages:
	$(MAKE) -C ocamldoc $@

partialclean::
	$(MAKE) -C ocamldoc clean

# The extra libraries

.PHONY: otherlibraries
otherlibraries: ocamltools
	$(MAKE) -C otherlibs all

.PHONY: otherlibrariesopt
otherlibrariesopt:
	$(MAKE) -C otherlibs allopt

partialclean::
	$(MAKE) -C otherlibs partialclean

clean::
	$(MAKE) -C otherlibs clean

# The replay debugger

.PHONY: ocamldebugger
ocamldebugger: ocamlc ocamlyacc ocamllex otherlibraries
	$(MAKE) -C debugger all

partialclean::
	$(MAKE) -C debugger clean

# Check that the native-code compiler is supported
.PHONY: checknative
checknative:
ifeq "$(ARCH)" "none"
checknative:
	$(error The native-code compiler is not supported on this platform)
else
	@
endif

# Check that the stack limit is reasonable (Unix-only)
.PHONY: checkstack
checkstack:
ifeq "$(UNIX_OR_WIN32)" "unix"
	if $(MKEXE) $(OUTPUTEXE)tools/checkstack$(EXE) tools/checkstack.c; \
	  then tools/checkstack$(EXE); \
	fi
	rm -f tools/checkstack$(EXE)
else
	@
endif

# Lint @since and @deprecated annotations

VERSIONS=$(shell git tag|grep '^[0-9]*.[0-9]*.[0-9]*$$'|grep -v '^[12].')
.PHONY: lintapidiff
lintapidiff:
	$(MAKE) -C tools lintapidiff.opt
	git ls-files -- 'otherlibs/*/*.mli' 'stdlib/*.mli' |\
	    grep -Ev internal\|obj\|spacetime\|stdLabels\|moreLabels |\
	    tools/lintapidiff.opt $(VERSIONS)

# The middle end.

compilerlibs/ocamlmiddleend.cma: $(MIDDLE_END)
	$(CAMLC) -a -o $@ $^
compilerlibs/ocamlmiddleend.cmxa: $(MIDDLE_END:%.cmo=%.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlmiddleend.cma \
	      compilerlibs/ocamlmiddleend.cmxa \
	      compilerlibs/ocamlmiddleend.$(A)

# Tools

.PHONY: ocamltools
ocamltools: ocamlc ocamllex compilerlibs/ocamlmiddleend.cma
	$(MAKE) -C tools all

.PHONY: ocamltoolsopt
ocamltoolsopt: ocamlopt
	$(MAKE) -C tools opt

.PHONY: ocamltoolsopt.opt
ocamltoolsopt.opt: ocamlc.opt ocamllex.opt compilerlibs/ocamlmiddleend.cmxa
	$(MAKE) -C tools opt.opt

partialclean::
	$(MAKE) -C tools clean

## Test compilation of backend-specific parts

partialclean::
	rm -f $(ARCH_SPECIFIC)

beforedepend:: $(ARCH_SPECIFIC)

# This rule provides a quick way to check that machine-dependent
# files compiles fine for a foreign architecture (passed as ARCH=xxx).

.PHONY: check_arch
check_arch:
	@echo "========= CHECKING asmcomp/$(ARCH) =============="
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*
	@$(MAKE) compilerlibs/ocamloptcomp.cma \
	            >/dev/null
	@rm -f $(ARCH_SPECIFIC) asmcomp/emit.ml asmcomp/*.cm*

.PHONY: check_all_arches
check_all_arches:
ifeq ($(ARCH64),true)
	@STATUS=0; \
	 for i in $(ARCHES); do \
	   $(MAKE) --no-print-directory check_arch ARCH=$$i || STATUS=1; \
	 done; \
	 exit $$STATUS
else
	 @echo "Architecture tests are disabled on 32-bit platforms."
endif

# The native toplevel

compilerlibs/ocamlopttoplevel.cmxa: $(OPTTOPLEVEL:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $^
partialclean::
	rm -f compilerlibs/ocamlopttoplevel.cmxa

# When the native toplevel executable has an extension (e.g. ".exe"),
# provide a phony 'ocamlnat' synonym

ifneq ($(EXE),)
.PHONY: ocamlnat
ocamlnat: ocamlnat$(EXE)
endif

ocamlnat$(EXE): compilerlibs/ocamlcommon.cmxa compilerlibs/ocamloptcomp.cmxa \
    compilerlibs/ocamlbytecomp.cmxa \
    otherlibs/dynlink/dynlink.cmxa \
    compilerlibs/ocamlopttoplevel.cmxa \
    $(OPTTOPLEVELSTART:.cmo=.cmx)
	$(CAMLOPT_CMD) $(LINKFLAGS) -linkall -o $@ $^

partialclean::
	rm -f ocamlnat$(EXE)

toplevel/opttoploop.cmx: otherlibs/dynlink/dynlink.cmxa

# The numeric opcodes

bytecomp/opcodes.ml: runtime/caml/instruct.h tools/make_opcodes
	runtime/ocamlrun tools/make_opcodes -opcodes < $< > $@

bytecomp/opcodes.mli: bytecomp/opcodes.ml
	$(CAMLC) -i $< > $@

tools/make_opcodes: tools/make_opcodes.mll
	$(MAKE) -C tools make_opcodes

partialclean::
	rm -f bytecomp/opcodes.ml
	rm -f bytecomp/opcodes.mli

beforedepend:: bytecomp/opcodes.ml bytecomp/opcodes.mli

ifneq "$(wildcard .git)" ""
include Makefile.dev
endif

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(OPTCOMPFLAGS) -c $<

partialclean::
	for d in utils parsing typing bytecomp asmcomp middle_end file_formats \
           lambda middle_end/closure middle_end/flambda \
           middle_end/flambda/base_types asmcomp/debug \
           middle_end/flambda/compilenv_deps \
           middle_end/flambda \
           middle_end/flambda/basic \
           middle_end/flambda/cmx \
           middle_end/flambda/from_lambda \
           middle_end/flambda/inlining \
           middle_end/flambda/naming \
           middle_end/flambda/parser \
           middle_end/flambda/simplify \
           middle_end/flambda/simplify/basic \
           middle_end/flambda/simplify/env \
           middle_end/flambda/simplify/typing_helpers \
           middle_end/flambda/terms \
           middle_end/flambda/to_cmm \
           middle_end/flambda/types \
           middle_end/flambda/types/basic \
           middle_end/flambda/types/env \
           middle_end/flambda/types/kinds \
           middle_end/flambda/types/structures \
           middle_end/flambda/types/type_of_kind \
           middle_end/flambda/types/type_of_kind/boilerplate \
           middle_end/flambda/unboxing \
           driver toplevel tools; do \
	  rm -f $$d/*.cm[ioxt] $$d/*.cmti $$d/*.annot $$d/*.$(S) \
	    $$d/*.$(O) $$d/*.$(SO); \
	done

.PHONY: depend
depend: beforedepend
	(for d in utils parsing typing bytecomp asmcomp middle_end \
         lambda file_formats middle_end/closure middle_end/flambda \
         middle_end/flambda/base_types asmcomp/debug \
         middle_end/flambda/compilenv_deps \
         middle_end/flambda \
         middle_end/flambda/basic \
         middle_end/flambda/cmx \
         middle_end/flambda/from_lambda \
         middle_end/flambda/inlining \
         middle_end/flambda/lifting \
         middle_end/flambda/naming \
         middle_end/flambda/parser \
         middle_end/flambda/simplify \
         middle_end/flambda/simplify/basic \
         middle_end/flambda/simplify/env \
         middle_end/flambda/simplify/typing_helpers \
         middle_end/flambda/terms \
         middle_end/flambda/to_cmm \
         middle_end/flambda/types \
         middle_end/flambda/types/basic \
         middle_end/flambda/types/env \
         middle_end/flambda/types/kinds \
         middle_end/flambda/types/structures \
         middle_end/flambda/types/type_of_kind \
         middle_end/flambda/types/type_of_kind/boilerplate \
         middle_end/flambda/unboxing \
         driver toplevel; \
         do $(CAMLDEP) $(DEPFLAGS) $(DEPINCLUDES) $$d/*.mli $$d/*.ml || exit; \
         done) > .depend

.PHONY: distclean
distclean: clean
	rm -f boot/ocamlrun boot/ocamlrun$(EXE) boot/camlheader \
	boot/*.cm* boot/libcamlrun.$(A) boot/ocamlc.opt
	rm -f Makefile.config runtime/caml/m.h runtime/caml/s.h
	rm -f tools/*.bak
	rm -f ocaml ocamlc
	rm -f testsuite/_log*

include .depend

Makefile.config Makefile.common:
	@echo "Please refer to the installation instructions:"
	@echo "- In file INSTALL for Unix systems."
	@echo "- In file README.win32.adoc for Windows systems."
	@echo "On Unix systems, if you've just unpacked the distribution,"
	@echo "something like"
	@echo "	./configure"
	@echo "	make"
	@echo "	make install"
	@echo "should work."
	@false
