(* TEST
*)

let test n b =
  if not b then Format.eprintf "failed %d !@." n

let test_consts () =
  test 1 (Int32.zero = 0l);
  test 2 (Int32.one = 1l);
  test 3 (Int32.minus_one = -1l);
  ()

let test_arith () =
  test 4 (Int32.add 2l 4l = 6l);
  test 5 (Int32.sub 6l 2l = 4l);
  test 6 (Int32.mul 6l 2l = 12l);
  test 7 (Int32.div 12l 2l = 6l);
  test 8 (Int32.rem 5l 2l = 1l);
  test 9 (Int32.succ 5l = 6l);
  test 10 (Int32.pred 5l = 4l);
  test 11 (Int32.abs (-5l) = 5l);
  test 12 (Int32.abs 5l = 5l);
  ()

let test_logops () =
  test 13 (Int32.logand 0xF0F0l 0xFFFFl = 0xF0F0l);
  test 14 (Int32.logor 0xF0FFl 0x0F0Fl = 0xFFFFl);
  test 15 (Int32.logxor 0xF0FFl 0x0F0Fl = 0xFFF0l);
  test 16 (Int32.lognot Int32.max_int = Int32.min_int);
  test 17 (Int32.shift_left 1l 4 = 16l);
  test 18 (Int32.shift_right 16l 4 = 1l);
  test 19 (Int32.shift_right (-16l) 4 = (-1l));
  test 20 (Int32.shift_right (-16l) 4 = (-1l));
  test 21 (Int32.shift_right_logical Int32.min_int 31 = 1l);
  ()

let test_equal () =
  test 22 (Int32.equal 1l 1l = true);
  test 23 (Int32.equal 1l 0l = false);
  ()

let test_compare () =
  test 24 (Int32.compare 3l 3l = 0);
  test 25 (Int32.compare 3l 4l = (-1));
  test 26 (Int32.compare 4l 3l = 1);
  test 27 (Int32.compare (-4l) 3l = -1);
  test 28 (Int32.compare 3l (-4l) = 1);
  ()

let test_float_conv () =
  test 29 (Int32.to_float 5l = 5.0);
  test 30 (Int32.of_float 5. = 5l);
  test 31 (Int32.of_float 5.9 = 5l);
  ()

let test_string_conv () =
  test 32 (Int32.to_string 50l = "50");
  ()

(*
   Test for the overflow behaviour, most notably that int32 are correctly
   sign-extended after operations which might overflow out of the int32 range
   on 64-bits architectures.
   Test in the basic/boxedints.ml file are somewhat biased in that they
   go through a polymorphic 'test' function that call the polymorphic
   equality, hence two things happen:
   - the int32 are boxed because they go through a function call,
     and it happens that the sign_extension is often part of the boxing
     operation
   - the comparison function for Int32s (which are custom blocks) only checks
     the low 32-bits of the stored value (which is in general correct, but
     in this case we need to check more).
   In flambda, boxing decisions are handled by flambda, and the translation
   from flambda to cmm thus does not systemically introduce boxing operations
   like what is done in cmmgen. Additionaly, flambda can simplify box-unbox
   trips to the identity on int32, which makes the flambda to cmm translation
   responsible for introducing sign extensions correctly after the operations
   on int32s (which operate in flambda on raw int32 rather than boxed int32
   that clambda and cmmgen uses).

   To correctly test the behavior of int32 overflow, we use two "tricks":
   - Nativeint.of_int32 is used to try and have an identity function
     on registers/machine-width values. This happens because currently
     the way int32 are represented is through a machine integers which is
     in the range of 32 bits integers, hence converison from 32 to
     machine-width should be the identity.
   - The equality test is inlined, so that the optimizer can skip the
     boxing and perform direct comparison on the integers. This isn't
     really technically needed, as the polymorphic comparison on nativeints
     should have the behaviour we want anyway.

   Finally, the behavior of overflow could be checked by simply chaining
   some operations on int32, e.g. [(2 * max_int) / 2] whose result would
   differ depending on whether the overflow of the multiplication was
   correctly handled, and this is usually the source of the bugs that occur
   in the wild when overflow is not handled correctly, but it also makes
   writing and reasoning about tests harder, hence the solution here with
   [Nativeint.of_int32].
*)
let test_overflow () =
  test 33 (
    Nativeint.of_int32 (Int32.add Int32.max_int 1l) =
    Nativeint.of_int32 Int32.min_int);
  test 34 (
    Nativeint.of_int32 (Int32.sub Int32.min_int 1l) =
    Nativeint.of_int32 Int32.max_int);
  test 35 (
    Nativeint.of_int32 (Int32.mul Int32.max_int 2l) =
    Nativeint.of_int32 (-2l));
  test 36 (
    Nativeint.of_int32 (Int32.shift_left Int32.max_int 1) =
    Nativeint.of_int32 (-2l));
  test 37 (
    Nativeint.of_int32 (
      Int32.of_int (
        Int32.to_int Int32.max_int + 1)) =
    Nativeint.of_int32 Int32.min_int);
  test 38 (
    Nativeint.of_int32 (
      Int32.of_float (
        Int32.to_float Int32.max_int +. 1.)) =
    Nativeint.of_int32 Int32.min_int);
  test 39 (
    (Int64.to_int32 (Int64.of_string "0x123456789ABCDEF0")) =
    (Int32.of_string "0x9ABCDEF0"));
  ()

let tests () =
  test_consts ();
  test_arith ();
  test_logops ();
  test_equal ();
  test_compare ();
  test_float_conv ();
  test_string_conv ();
  test_overflow ();
  ()

let () = tests ()

