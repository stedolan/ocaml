
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | UNREACHABLE
    | UNDERSCORE
    | UIDENT of (
# 83 "flambda_parser.mly"
       (string)
# 13 "flambda_parser.ml"
  )
    | TAG
    | SWITCH
    | STUB
    | STAR
    | SEMICOLON
    | RPAREN
    | ROOT
    | REC
    | RBRACKET
    | RBRACE
    | PLUSDOT
    | PLUS
    | OPAQUE
    | MUT
    | MINUSGREATER
    | MINUSDOT
    | MINUS
    | LPAREN
    | LIDENT of (
# 64 "flambda_parser.mly"
       (string)
# 36 "flambda_parser.ml"
  )
    | LETK
    | LET
    | LBRACKET
    | LBRACE
    | IS_INT
    | INT of (
# 59 "flambda_parser.mly"
       (string * char option)
# 46 "flambda_parser.ml"
  )
    | IN
    | HCF
    | GET_FIELD
    | FLOAT of (
# 54 "flambda_parser.mly"
       (string * char option)
# 54 "flambda_parser.ml"
  )
    | EXN
    | EQUAL
    | EOF
    | EFFECT
    | DOT
    | DEF
    | CONT
    | COMMA
    | COLONEQUAL
    | COLON
    | CODE
    | CLOSURE
    | CCALL
    | BLOCK
    | BANG
    | AROBASE
    | APPLY
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState213
  | MenhirState209
  | MenhirState206
  | MenhirState202
  | MenhirState198
  | MenhirState196
  | MenhirState193
  | MenhirState191
  | MenhirState190
  | MenhirState189
  | MenhirState188
  | MenhirState187
  | MenhirState185
  | MenhirState183
  | MenhirState180
  | MenhirState179
  | MenhirState178
  | MenhirState176
  | MenhirState173
  | MenhirState171
  | MenhirState170
  | MenhirState169
  | MenhirState165
  | MenhirState163
  | MenhirState157
  | MenhirState154
  | MenhirState148
  | MenhirState145
  | MenhirState142
  | MenhirState134
  | MenhirState132
  | MenhirState129
  | MenhirState128
  | MenhirState126
  | MenhirState123
  | MenhirState121
  | MenhirState118
  | MenhirState116
  | MenhirState114
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState105
  | MenhirState103
  | MenhirState102
  | MenhirState101
  | MenhirState96
  | MenhirState95
  | MenhirState93
  | MenhirState92
  | MenhirState91
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState76
  | MenhirState73
  | MenhirState71
  | MenhirState70
  | MenhirState65
  | MenhirState63
  | MenhirState56
  | MenhirState53
  | MenhirState49
  | MenhirState43
  | MenhirState40
  | MenhirState33
  | MenhirState29
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState17
  | MenhirState12
  | MenhirState9
  | MenhirState8
  | MenhirState5
  | MenhirState1
  | MenhirState0

# 1 "flambda_parser.mly"
  
open Fexpr

let make_loc (startpos, endpos) = Lambda.of_raw_location {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, Some 't' -> s
  | _, _ ->
    failwith "Tagged immediates must have modifier 't'"

let make_const_int (i, m) : Fexpr.const =
  match m with
  | None -> Naked_nativeint (Int64.of_string i)
  | Some 'u' -> Naked_immediate i
  | Some 't' -> Tagged_immediate i
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)

let make_const_float (i, m) =
  match m with
  | None -> Naked_float (float_of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown float modifier %c" c)

# 207 "flambda_parser.ml"

let rec _menhir_goto_tags_to_sizes : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tags_to_sizes -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv857)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv853)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv851)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_switch_sort = 
# 147 "flambda_parser.mly"
                                                        ( Tag { tags_to_sizes } )
# 232 "flambda_parser.ml"
             in
            _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv852)) : 'freshtv854)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv855)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv856)) : 'freshtv858)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv861 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv859 * _menhir_state * 'tv_tag_to_size)) * _menhir_state * 'tv_tags_to_sizes) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)), _, (tags_to_sizes : 'tv_tags_to_sizes)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_tags_to_sizes = 
# 144 "flambda_parser.mly"
                                                                  ( tag_to_size :: tags_to_sizes )
# 252 "flambda_parser.ml"
         in
        _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv860)) : 'freshtv862)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_static_structure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_static_structure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv839 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 267 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv837 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 273 "flambda_parser.ml"
        )) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 278 "flambda_parser.ml"
        ))), _, (xs : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : 'tv_nonempty_list_static_structure_ = 
# 223 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 283 "flambda_parser.ml"
         in
        _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv838)) : 'freshtv840)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv845 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv841 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState209 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState209 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState209 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState209 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState209
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState209) : 'freshtv842)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv843 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv844)) : 'freshtv846)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv849 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv847 * _menhir_state * 'tv_nonempty_list_static_structure_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (static : 'tv_nonempty_list_static_structure_)) = _menhir_stack in
        let _v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 350 "flambda_parser.ml"
        ) = 
# 267 "flambda_parser.mly"
      ( { computation = None; static_structure = static } )
# 354 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv848)) : 'freshtv850)
    | _ ->
        _menhir_fail ()

and _menhir_goto_return_kinds : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_kinds -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv831 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv829 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((k : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_return_arity = 
# 194 "flambda_parser.mly"
                           ( Some k )
# 377 "flambda_parser.ml"
         in
        _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v) : 'freshtv830)) : 'freshtv832)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv835 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_return_kinds) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv833 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((t : 'tv_return_kinds) : 'tv_return_kinds) = _v in
        ((let (_menhir_stack, _menhir_s, (k : 'tv_kind)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_return_kinds = 
# 190 "flambda_parser.mly"
                                    ( k :: t )
# 394 "flambda_parser.ml"
         in
        _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v) : 'freshtv834)) : 'freshtv836)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv819 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv817 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_variable)), _, (xs : 'tv_list_variable_)) = _menhir_stack in
        let _v : 'tv_list_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 413 "flambda_parser.ml"
         in
        _menhir_goto_list_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv818)) : 'freshtv820)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv827) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv823) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv821) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, (v : 'tv_list_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_closure_elements = 
# 291 "flambda_parser.mly"
                                    ( v )
# 434 "flambda_parser.ml"
             in
            _menhir_goto_closure_elements _menhir_env _menhir_stack _v) : 'freshtv822)) : 'freshtv824)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv825) * _menhir_state * 'tv_list_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv826)) : 'freshtv828)
    | _ ->
        _menhir_fail ()

and _menhir_reduce102 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tags_to_sizes = 
# 142 "flambda_parser.mly"
    ( [] )
# 452 "flambda_parser.ml"
     in
    _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v

and _menhir_run34 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 459 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv813 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 471 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv809 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 481 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 59 "flambda_parser.mly"
       (string * char option)
# 487 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv807 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 495 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            let (_endpos_size_ : Lexing.position) = _endpos in
            let ((size : (
# 59 "flambda_parser.mly"
       (string * char option)
# 501 "flambda_parser.ml"
            )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 505 "flambda_parser.ml"
            )) = _v in
            let (_startpos_size_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _endpos_tag_, _menhir_s, (tag : (
# 59 "flambda_parser.mly"
       (string * char option)
# 511 "flambda_parser.ml"
            )), _startpos_tag_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_tag_to_size = 
# 135 "flambda_parser.mly"
                               (
  let (tag, _) = tag in
  let (size, _) = size in
  int_of_string tag, int_of_string size
)
# 521 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv805) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_tag_to_size) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv803 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv797 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState40 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RBRACKET ->
                    _menhir_reduce102 _menhir_env (Obj.magic _menhir_stack) MenhirState40
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv798)
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv799 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (tag_to_size : 'tv_tag_to_size)) = _menhir_stack in
                let _v : 'tv_tags_to_sizes = 
# 143 "flambda_parser.mly"
                              ( [ tag_to_size ] )
# 554 "flambda_parser.ml"
                 in
                _menhir_goto_tags_to_sizes _menhir_env _menhir_stack _menhir_s _v) : 'freshtv800)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv801 * _menhir_state * 'tv_tag_to_size) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv802)) : 'freshtv804)) : 'freshtv806)) : 'freshtv808)) : 'freshtv810)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv811 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 571 "flambda_parser.ml"
            ) * Lexing.position)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv812)) : 'freshtv814)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv815 * Lexing.position * _menhir_state * (
# 59 "flambda_parser.mly"
       (string * char option)
# 582 "flambda_parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv816)

and _menhir_goto_switch_sort : _menhir_env -> 'ttv_tail -> 'tv_switch_sort -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv795 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState43 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv796)

and _menhir_goto_variable_opt : _menhir_env -> 'ttv_tail -> 'tv_variable_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv793) = Obj.magic _menhir_stack in
    let (_v : 'tv_variable_opt) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv791) = Obj.magic _menhir_stack in
    let ((v : 'tv_variable_opt) : 'tv_variable_opt) = _v in
    ((let _v : 'tv_kinded_variable_opt = 
# 326 "flambda_parser.mly"
                     ( v, None )
# 619 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv789) = _menhir_stack in
    let (_v : 'tv_kinded_variable_opt) = _v in
    ((let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv787 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv783 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | BLOCK ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState163
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState163 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163) : 'freshtv784)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv785 * _menhir_state) * 'tv_kinded_variable_opt) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv786)) : 'freshtv788)) : 'freshtv790)) : 'freshtv792)) : 'freshtv794)

and _menhir_goto_list_kinded_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_kinded_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv777 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv773 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv771 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_kinded_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_args = 
# 299 "flambda_parser.mly"
                                       ( v )
# 684 "flambda_parser.ml"
             in
            _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv772)) : 'freshtv774)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv775 * _menhir_state) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv776)) : 'freshtv778)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv781 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * _menhir_state * 'tv_kinded_variable) * _menhir_state * 'tv_list_kinded_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_kinded_variable)), _, (xs : 'tv_list_kinded_variable_)) = _menhir_stack in
        let _v : 'tv_list_kinded_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 703 "flambda_parser.ml"
         in
        _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv780)) : 'freshtv782)
    | _ ->
        _menhir_fail ()

and _menhir_goto_closure_elements : _menhir_env -> 'ttv_tail -> 'tv_closure_elements -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv769 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MINUSGREATER ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv765 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState101 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv766)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv767 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv768)) : 'freshtv770)

and _menhir_goto_list_typed_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_typed_variable_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv755 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv753 * _menhir_state * 'tv_typed_variable) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_typed_variable)), _, (xs : 'tv_list_typed_variable_)) = _menhir_stack in
        let _v : 'tv_list_typed_variable_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 750 "flambda_parser.ml"
         in
        _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv754)) : 'freshtv756)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv763 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv759 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv757 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_list_typed_variable_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_typed_args = 
# 295 "flambda_parser.mly"
                                      ( v )
# 771 "flambda_parser.ml"
             in
            _menhir_goto_typed_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv758)) : 'freshtv760)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv761 * _menhir_state) * _menhir_state * 'tv_list_typed_variable_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv762)) : 'freshtv764)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_of_kind_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_of_kind_value_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv737 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 793 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv735 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 799 "flambda_parser.ml"
        )) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 804 "flambda_parser.ml"
        ))), _, (xs : 'tv_list_of_kind_value_)) = _menhir_stack in
        let _v : 'tv_list_of_kind_value_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 809 "flambda_parser.ml"
         in
        _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv736)) : 'freshtv738)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv751 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv747 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv745 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (s : 'tv_symbol)), _, (t : 'tv_tag)), _, (elts : 'tv_list_of_kind_value_)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 832 "flambda_parser.ml"
            ) = 
# 304 "flambda_parser.mly"
    ( ( s, None, Block (t, Immutable, elts) ) )
# 836 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv743) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 844 "flambda_parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv741 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 851 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState206 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv739 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 863 "flambda_parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 868 "flambda_parser.ml"
                ))) = _menhir_stack in
                let _v : 'tv_nonempty_list_static_structure_ = 
# 221 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [ x ] )
# 873 "flambda_parser.ml"
                 in
                _menhir_goto_nonempty_list_static_structure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv740)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206) : 'freshtv742)) : 'freshtv744)) : 'freshtv746)) : 'freshtv748)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv749 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_of_kind_value_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv750)) : 'freshtv752)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_simple_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_simple_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv717 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv715 * _menhir_state * 'tv_simple) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_simple)), _, (xs : 'tv_list_simple_)) = _menhir_stack in
        let _v : 'tv_list_simple_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 903 "flambda_parser.ml"
         in
        _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv716)) : 'freshtv718)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv725 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv721 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv719 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_list_simple_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_args = 
# 331 "flambda_parser.mly"
                              ( s )
# 924 "flambda_parser.ml"
             in
            _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv720)) : 'freshtv722)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv723 * _menhir_state) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv724)) : 'freshtv726)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv733 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv729 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv727 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (t : 'tv_tag)), _, (elts : 'tv_list_simple_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_named = 
# 171 "flambda_parser.mly"
                                               ( Prim (Block (t, Immutable, elts)) )
# 953 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv728)) : 'freshtv730)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv731 * _menhir_state) * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_list_simple_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv732)) : 'freshtv734)
    | _ ->
        _menhir_fail ()

and _menhir_goto_const : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_const -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv713) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_const) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv711) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : 'tv_const) : 'tv_const) = _v in
    ((let _v : 'tv_simple = 
# 347 "flambda_parser.mly"
              ( Const c )
# 979 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv712)) : 'freshtv714)

and _menhir_goto_return_arity : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_return_arity -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv697 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv693 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv694)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv695 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv696)) : 'freshtv698)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv703 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv699 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState105 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105) : 'freshtv700)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv701 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv709 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv705 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv706)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv707 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv708)) : 'freshtv710)
    | _ ->
        _menhir_fail ()

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_kind = 
# 186 "flambda_parser.mly"
    ( None )
# 1130 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv691) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_kind) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv689 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv685 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL | LBRACE | MINUSGREATER ->
            _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | COMMA ->
            _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState26
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv686)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv687 * _menhir_state * 'tv_kind) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv688)) : 'freshtv690)) : 'freshtv692)

and _menhir_reduce82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_kinds = 
# 189 "flambda_parser.mly"
    ( [] )
# 1169 "flambda_parser.ml"
     in
    _menhir_goto_return_kinds _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_andk : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_andk -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv681 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv679 * _menhir_state) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (h : 'tv_continuation_handler)), _, (t : 'tv_andk)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_andk = 
# 262 "flambda_parser.mly"
                                          ( h :: t )
# 1187 "flambda_parser.ml"
         in
        _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v) : 'freshtv680)) : 'freshtv682)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv683 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | BANG ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | BLOCK ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | CCALL ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState173 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState173 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState173
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState173) : 'freshtv684)
    | _ ->
        _menhir_fail ()

and _menhir_reduce53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 1240 "flambda_parser.ml"
     in
    _menhir_goto_list_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce87 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_variable -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
    let _v : 'tv_simple = 
# 346 "flambda_parser.mly"
                 ( Var v )
# 1250 "flambda_parser.ml"
     in
    _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_exn_and_stub : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exn_and_stub -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv677 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv678)

and _menhir_goto_infix_binop : _menhir_env -> 'ttv_tail -> 'tv_infix_binop -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv675 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState145 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv676)

and _menhir_goto_named : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_named -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState209 | MenhirState185 | MenhirState29 | MenhirState173 | MenhirState73 | MenhirState165 | MenhirState81 | MenhirState154 | MenhirState148 | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv667 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv663 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState148
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv665 * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv666)) : 'freshtv668)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv673 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv669 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState165 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState165
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165) : 'freshtv670)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv671 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv672)) : 'freshtv674)
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv661) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1417 "flambda_parser.ml"
    ) = 
# 198 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 1421 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv662)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS_INT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv649) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv647) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_switch_sort = 
# 149 "flambda_parser.mly"
           ( Is_int )
# 1441 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv648)) : 'freshtv650)
    | TAG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv655) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv651) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce102 _menhir_env (Obj.magic _menhir_stack) MenhirState33
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv653) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv654)) : 'freshtv656)
    | FLOAT _ | INT _ | LIDENT _ | UIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv657) = Obj.magic _menhir_stack in
        ((let _v : 'tv_switch_sort = 
# 148 "flambda_parser.mly"
    ( Int )
# 1476 "flambda_parser.ml"
         in
        _menhir_goto_switch_sort _menhir_env _menhir_stack _v) : 'freshtv658)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv659 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv660)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv645) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unop = 
# 153 "flambda_parser.mly"
           ( Opaque_identity )
# 1497 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv643) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_unop) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv641 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv642)) : 'freshtv644)) : 'freshtv646)

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EXN | LIDENT _ | STUB ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv631) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 64 "flambda_parser.mly"
       (string)
# 1550 "flambda_parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv629) = Obj.magic _menhir_stack in
        let (_endpos_e_ : Lexing.position) = _endpos in
        let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 1560 "flambda_parser.ml"
        )) : (
# 64 "flambda_parser.mly"
       (string)
# 1564 "flambda_parser.ml"
        )) = _v in
        let (_startpos_e_ : Lexing.position) = _startpos in
        ((let _v : 'tv_variable_opt = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 368 "flambda_parser.mly"
               ( Some (e, make_loc (_startpos, _endpos)) )
# 1572 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv630)) : 'freshtv632)
    | MUT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv633 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState76 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv634)
    | UNDERSCORE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv637) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv635) = Obj.magic _menhir_stack in
        ((let _1 = () in
        let _v : 'tv_variable_opt = 
# 367 "flambda_parser.mly"
               ( None )
# 1597 "flambda_parser.ml"
         in
        _menhir_goto_variable_opt _menhir_env _menhir_stack _v) : 'freshtv636)) : 'freshtv638)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv639 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv640)

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv627) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1618 "flambda_parser.ml"
    ) = 
# 197 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 1622 "flambda_parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)

and _menhir_run83 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LIDENT _ ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv623 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv619) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 64 "flambda_parser.mly"
       (string)
# 1673 "flambda_parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv617) = Obj.magic _menhir_stack in
            let (_endpos_s_ : Lexing.position) = _endpos in
            let ((s : (
# 64 "flambda_parser.mly"
       (string)
# 1683 "flambda_parser.ml"
            )) : (
# 64 "flambda_parser.mly"
       (string)
# 1687 "flambda_parser.ml"
            )) = _v in
            let (_startpos_s_ : Lexing.position) = _startpos in
            ((let _v : 'tv_csymbol = let _endpos = _endpos_s_ in
            let _startpos = _startpos_s_ in
            
# 359 "flambda_parser.mly"
               ( s, make_loc (_startpos, _endpos) )
# 1695 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv615) = _menhir_stack in
            let (_v : 'tv_csymbol) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv613 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv609 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LPAREN ->
                    _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | COLON | MINUSGREATER ->
                    _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv610)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv611 * _menhir_state)) * 'tv_csymbol) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv612)) : 'freshtv614)) : 'freshtv616)) : 'freshtv618)) : 'freshtv620)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv621 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv622)) : 'freshtv624)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv625 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv626)

and _menhir_run116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | BANG ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | BLOCK ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | CCALL ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState185
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv602)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv607 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv603 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193) : 'freshtv604)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv605 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv606)) : 'freshtv608)
    | _ ->
        _menhir_fail ()

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_kinded_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 1862 "flambda_parser.ml"
     in
    _menhir_goto_list_kinded_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_simple_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv591 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv589 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (s : 'tv_simple_args)) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 1880 "flambda_parser.ml"
        ) = 
# 199 "flambda_parser.mly"
                                          ( Apply_cont (c, None, s) )
# 1884 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv590)) : 'freshtv592)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv593 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | MINUSGREATER ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv594)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv599 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv595 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState128 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128) : 'freshtv596)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv597 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv598)) : 'freshtv600)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typed_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typed_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv573 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv569 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState17 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv570)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv571 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv572)) : 'freshtv574)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv579 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv575 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv577 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv578)) : 'freshtv580)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv587 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv581) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState95 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RBRACKET ->
                _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv582)
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv583) = Obj.magic _menhir_stack in
            ((let _v : 'tv_closure_elements = 
# 292 "flambda_parser.mly"
    ( [] )
# 2039 "flambda_parser.ml"
             in
            _menhir_goto_closure_elements _menhir_env _menhir_stack _v) : 'freshtv584)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv585 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)) : 'freshtv588)
    | _ ->
        _menhir_fail ()

and _menhir_reduce51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_typed_variable_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2057 "flambda_parser.ml"
     in
    _menhir_goto_list_typed_variable_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_SEMICOLON_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_SEMICOLON_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv567) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_option_SEMICOLON_) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv565) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : 'tv_option_SEMICOLON_) : 'tv_option_SEMICOLON_) = _v in
    ((let _v : 'tv_switch = 
# 181 "flambda_parser.mly"
                      ( [] )
# 2074 "flambda_parser.ml"
     in
    _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv566)) : 'freshtv568)

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_of_kind_value_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2083 "flambda_parser.ml"
     in
    _menhir_goto_list_of_kind_value_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run199 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 2090 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv563) = Obj.magic _menhir_stack in
    let (_endpos_i_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2101 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2105 "flambda_parser.ml"
    )) = _v in
    let (_startpos_i_ : Lexing.position) = _startpos in
    ((let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2111 "flambda_parser.ml"
    ) = let _endpos = _endpos_i_ in
    let _startpos = _startpos_i_ in
    
# 314 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 2117 "flambda_parser.ml"
     in
    _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv564)

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_simple_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 2126 "flambda_parser.ml"
     in
    _menhir_goto_list_simple_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 2133 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv561) = Obj.magic _menhir_stack in
    let (_endpos_c_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2144 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 2148 "flambda_parser.ml"
    )) = _v in
    let (_startpos_c_ : Lexing.position) = _startpos in
    ((let _v : 'tv_const = 
# 335 "flambda_parser.mly"
            ( make_const_int c )
# 2154 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv562)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 54 "flambda_parser.mly"
       (string * char option)
# 2161 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((c : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2171 "flambda_parser.ml"
    )) : (
# 54 "flambda_parser.mly"
       (string * char option)
# 2175 "flambda_parser.ml"
    )) = _v in
    ((let _v : 'tv_const = 
# 336 "flambda_parser.mly"
              ( make_const_float c )
# 2180 "flambda_parser.ml"
     in
    _menhir_goto_const _menhir_env _menhir_stack _menhir_s _v) : 'freshtv560)

and _menhir_reduce80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_return_arity = 
# 193 "flambda_parser.mly"
    ( None )
# 2189 "flambda_parser.ml"
     in
    _menhir_goto_return_arity _menhir_env _menhir_stack _menhir_s _v

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL | LBRACE | MINUSGREATER ->
        _menhir_reduce82 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | COMMA ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_definition : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 2211 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv557 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 2220 "flambda_parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv555 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((def : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 2228 "flambda_parser.ml"
    )) : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 2232 "flambda_parser.ml"
    )) = _v in
    ((let (((_menhir_stack, _menhir_s), _, (recu : 'tv_recursive)), _, (exn_cont : 'tv_option_exn_continuation_)) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_program_body_elt = 
# 106 "flambda_parser.mly"
    ( let def =
        match def.computation with
        | None -> def
        | Some comput ->
          { def with computation =
            Some { comput with exception_cont = exn_cont } }
      in
      Define_symbol (recu, def) )
# 2246 "flambda_parser.ml"
     in
    _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)) : 'freshtv558)

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_andk = 
# 263 "flambda_parser.mly"
    ( [] )
# 2255 "flambda_parser.ml"
     in
    _menhir_goto_andk _menhir_env _menhir_stack _menhir_s _v

and _menhir_run170 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | STUB ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | LIDENT _ ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState170
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170

and _menhir_goto_separated_nonempty_list_AND_closure_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_AND_closure_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv549 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | APPLY ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | BANG ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | BLOCK ->
            _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | CCALL ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | CLOSURE ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | CONT ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
        | HCF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | LETK ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | OPAQUE ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | SWITCH ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UNREACHABLE ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState154
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv550)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv553 * _menhir_state * 'tv_closure)) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv551 * _menhir_state * 'tv_closure)) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_closure)), _, (xs : 'tv_separated_nonempty_list_AND_closure_)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_AND_closure_ = 
# 243 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 2332 "flambda_parser.ml"
         in
        _menhir_goto_separated_nonempty_list_AND_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
    | _ ->
        _menhir_fail ()

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 64 "flambda_parser.mly"
       (string)
# 2341 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 2352 "flambda_parser.ml"
    )) : (
# 64 "flambda_parser.mly"
       (string)
# 2356 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_variable = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 363 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 2364 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_variable) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState12 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv503 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (param : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_typed_variable = 
# 318 "flambda_parser.mly"
                     ( { param; ty = () } )
# 2381 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv501) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_typed_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv499 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState12 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12) : 'freshtv500)) : 'freshtv502)) : 'freshtv504)) : 'freshtv506)
    | MenhirState145 | MenhirState142 | MenhirState134 | MenhirState132 | MenhirState118 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv507 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        (_menhir_reduce87 _menhir_env (Obj.magic _menhir_stack) : 'freshtv508)
    | MenhirState183 | MenhirState180 | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv521 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv519 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_kinded_variable = 
# 322 "flambda_parser.mly"
                 ( v, None )
# 2414 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv517) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_kinded_variable) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState76 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv513 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv509 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                | INT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79) : 'freshtv510)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv511 * _menhir_state)) * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)) : 'freshtv514)
        | MenhirState183 | MenhirState180 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv515 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState183
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv516)
        | _ ->
            _menhir_fail ()) : 'freshtv518)) : 'freshtv520)) : 'freshtv522)
    | MenhirState157 | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | LBRACKET | MINUSGREATER ->
            _menhir_reduce106 _menhir_env (Obj.magic _menhir_stack) MenhirState93
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv524)
    | MenhirState96 | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACKET ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack) MenhirState96
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv526)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv529 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv527 * _menhir_state) * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (v : 'tv_variable)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_named = 
# 172 "flambda_parser.mly"
                      ( Read_mutable v )
# 2507 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv528)) : 'freshtv530)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : 'tv_name = 
# 341 "flambda_parser.mly"
                 ( (Var v:name) )
# 2519 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv532)) : 'freshtv534)
    | MenhirState209 | MenhirState185 | MenhirState29 | MenhirState173 | MenhirState73 | MenhirState165 | MenhirState163 | MenhirState81 | MenhirState154 | MenhirState148 | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv539 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONEQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv535 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv536)
        | DOT | IN | MINUS | MINUSDOT | PLUS | PLUSDOT | SEMICOLON ->
            _menhir_reduce87 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv537 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
    | MenhirState202 | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv543 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv541 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (v : 'tv_variable)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2564 "flambda_parser.ml"
        ) = 
# 313 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 2568 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv542)) : 'freshtv544)
    | _ ->
        _menhir_fail ()) : 'freshtv546)) : 'freshtv548)

and _menhir_reduce17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_exn_and_stub = 
# 247 "flambda_parser.mly"
    ( false, false )
# 2579 "flambda_parser.ml"
     in
    _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv491 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 250 "flambda_parser.mly"
             ( true, true )
# 2601 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv492)) : 'freshtv494)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 248 "flambda_parser.mly"
         ( false, true )
# 2612 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv496)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STUB ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 251 "flambda_parser.mly"
             ( true, true )
# 2641 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv484)) : 'freshtv486)
    | LIDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_and_stub = 
# 249 "flambda_parser.mly"
        ( true, false )
# 2652 "flambda_parser.ml"
         in
        _menhir_goto_exn_and_stub _menhir_env _menhir_stack _menhir_s _v) : 'freshtv488)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)

and _menhir_goto_of_kind_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2666 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv481 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 2674 "flambda_parser.ml"
    )) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        _menhir_run199 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState202 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState202 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState202 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState202
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202) : 'freshtv482)

and _menhir_goto_name : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_name -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv479 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | MINUSGREATER ->
        _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState126
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv480)

and _menhir_goto_simple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv411 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | RBRACE ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState49
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv412)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv413 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)) : 'freshtv416)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv421 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv417 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | APPLY ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | BANG ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | BLOCK ->
                _menhir_run116 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CCALL ->
                _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CLOSURE ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | CONT ->
                _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | HCF ->
                _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LETK ->
                _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | OPAQUE ->
                _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | SWITCH ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UNREACHABLE ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv419 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | MenhirState118 | MenhirState86 | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv423 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FLOAT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
        | INT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState86 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86) : 'freshtv424)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * 'tv_variable)) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (v : 'tv_variable)), _, (s : 'tv_simple)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_named = 
# 173 "flambda_parser.mly"
                                       ( Assign { being_assigned = v; new_value = s } )
# 2827 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)) : 'freshtv428)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * _menhir_state * 'tv_unop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (u : 'tv_unop)), _, (a : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 168 "flambda_parser.mly"
                        ( Prim (Unop (u, a)) )
# 2839 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)) : 'freshtv432)
    | MenhirState209 | MenhirState185 | MenhirState29 | MenhirState173 | MenhirState73 | MenhirState165 | MenhirState163 | MenhirState81 | MenhirState154 | MenhirState148 | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv459 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv437 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv433 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | FLOAT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
                | INT _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UIDENT _v ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv434)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv435 * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)) : 'freshtv438)
        | MINUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv439) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 158 "flambda_parser.mly"
          ( Minus )
# 2889 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv440)) : 'freshtv442)
        | MINUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv445) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 159 "flambda_parser.mly"
             ( Minusdot )
# 2902 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv444)) : 'freshtv446)
        | PLUS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv447) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 156 "flambda_parser.mly"
         ( Plus )
# 2915 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv448)) : 'freshtv450)
        | PLUSDOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv453) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv451) = Obj.magic _menhir_stack in
            ((let _1 = () in
            let _v : 'tv_infix_binop = 
# 157 "flambda_parser.mly"
            ( Plusdot )
# 2928 "flambda_parser.ml"
             in
            _menhir_goto_infix_binop _menhir_env _menhir_stack _v) : 'freshtv452)) : 'freshtv454)
        | IN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv455 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (s : 'tv_simple)) = _menhir_stack in
            let _v : 'tv_named = 
# 167 "flambda_parser.mly"
               ( Simple s )
# 2938 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv457 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)) : 'freshtv460)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv473 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv469 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv467 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (a : 'tv_simple)), _, (f : 'tv_simple)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _v : 'tv_binop = 
# 164 "flambda_parser.mly"
    ( Binop (Block_load (Block Value, Immutable), a, f) )
# 2967 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv465) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv463) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_binop) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv461) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((b : 'tv_binop) : 'tv_binop) = _v in
            ((let _v : 'tv_named = 
# 170 "flambda_parser.mly"
              ( Prim b )
# 2984 "flambda_parser.ml"
             in
            _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)) : 'freshtv466)) : 'freshtv468)) : 'freshtv470)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv471 * _menhir_state * 'tv_simple))) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)) : 'freshtv474)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv477 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv475 * _menhir_state * 'tv_simple) * 'tv_infix_binop) * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (a1 : 'tv_simple)), (b : 'tv_infix_binop)), _, (a2 : 'tv_simple)) = _menhir_stack in
        let _v : 'tv_named = 
# 169 "flambda_parser.mly"
                                            ( Prim (Infix_binop (b, a1, a2)) )
# 3003 "flambda_parser.ml"
         in
        _menhir_goto_named _menhir_env _menhir_stack _menhir_s _v) : 'freshtv476)) : 'freshtv478)
    | _ ->
        _menhir_fail ()

and _menhir_goto_program_body_elt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_program_body_elt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv409 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run187 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | EFFECT ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | EOF ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState213
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213) : 'freshtv410)

and _menhir_reduce4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 300 "flambda_parser.mly"
    ( [] )
# 3037 "flambda_parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run180 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState180
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180

and _menhir_reduce89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_simple_args = 
# 330 "flambda_parser.mly"
    ( [] )
# 3061 "flambda_parser.ml"
     in
    _menhir_goto_simple_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run85 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FLOAT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | INT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_reduce106 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_typed_args = 
# 296 "flambda_parser.mly"
    ( [] )
# 3091 "flambda_parser.ml"
     in
    _menhir_goto_typed_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState9 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        _menhir_reduce51 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_goto_switch : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_switch -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_switch_case)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (h : 'tv_switch_case)), _, (t : 'tv_switch)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch = 
# 183 "flambda_parser.mly"
                                         ( h :: t )
# 3124 "flambda_parser.ml"
         in
        _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)) : 'freshtv400)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv407 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv403 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv401 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), (sort : 'tv_switch_sort)), _, (scrutinee : 'tv_simple)), _, (cases : 'tv_switch)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3146 "flambda_parser.ml"
            ) = 
# 201 "flambda_parser.mly"
    ( Switch {scrutinee; sort; cases} )
# 3150 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)) : 'freshtv404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv405 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) * _menhir_state * 'tv_switch) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)) : 'freshtv408)
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_SEMICOLON_ = 
# 114 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( None )
# 3168 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let x = () in
    let _v : 'tv_option_SEMICOLON_ = 
# 116 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( Some x )
# 3182 "flambda_parser.ml"
     in
    _menhir_goto_option_SEMICOLON_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)

and _menhir_run51 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 59 "flambda_parser.mly"
       (string * char option)
# 3189 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393) = Obj.magic _menhir_stack in
    let (_endpos_tag_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((tag : (
# 59 "flambda_parser.mly"
       (string * char option)
# 3200 "flambda_parser.ml"
    )) : (
# 59 "flambda_parser.mly"
       (string * char option)
# 3204 "flambda_parser.ml"
    )) = _v in
    let (_startpos_tag_ : Lexing.position) = _startpos in
    ((let _v : 'tv_tag = let _endpos = _endpos_tag_ in
    let _startpos = _startpos_tag_ in
    
# 308 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 3212 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv391) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_tag) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState56 | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUSGREATER ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState53 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53) : 'freshtv374)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375 * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv383 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv379 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | FLOAT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | INT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118) : 'freshtv380)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv381 * _menhir_state) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv389 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv385 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run199 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIDENT _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UIDENT _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState198 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | RPAREN ->
                _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState198
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState198) : 'freshtv386)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv387 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)) : 'freshtv390)
    | _ ->
        _menhir_fail ()) : 'freshtv392)) : 'freshtv394)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_option_exn_continuation_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_option_exn_continuation_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv363 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | EQUAL ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22) : 'freshtv364)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv365 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LBRACE ->
            _menhir_reduce80 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv366)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | APPLY | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState179
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179) : 'freshtv368)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv371 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LETK ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState189 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LIDENT _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190) : 'freshtv370)
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189) : 'freshtv372)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3395 "flambda_parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv289 * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3405 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3411 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (defining_expr : 'tv_named)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3416 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3422 "flambda_parser.ml"
        ) = 
# 206 "flambda_parser.mly"
      ( Let { var = None; kind = None; defining_expr; body } )
# 3426 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv288)) : 'freshtv290)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv307 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3434 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv303 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3444 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv301 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3451 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s, (name : 'tv_variable)), _, (params : 'tv_typed_args)), (closure_vars : 'tv_closure_elements)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3456 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _4 = () in
            let _v : 'tv_closure = 
# 244 "flambda_parser.mly"
    ( { name; params; closure_vars; ret_cont; exn_cont; ret_arity; expr } )
# 3464 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv299) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_closure) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv297 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | AND ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LIDENT _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState157 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState157) : 'freshtv292)
            | APPLY | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv293 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_closure)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_AND_closure_ = 
# 241 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [ x ] )
# 3495 "flambda_parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_AND_closure_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv294)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv295 * _menhir_state * 'tv_closure) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)) : 'freshtv298)) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv305 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3512 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)) : 'freshtv308)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv311 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3521 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv309 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3527 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (r : 'tv_recursive)), _, (c : 'tv_separated_nonempty_list_AND_closure_)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3532 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3538 "flambda_parser.ml"
        ) = 
# 236 "flambda_parser.mly"
    ( Let_closure { closures = c; body; recursive = r } )
# 3542 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)) : 'freshtv312)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv315 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3550 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv313 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3556 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), _, (v : 'tv_kinded_variable)), _, (initial_value : 'tv_simple)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3561 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3570 "flambda_parser.ml"
        ) = 
# 208 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let_mutable { var; kind; initial_value; body } )
# 3575 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)) : 'freshtv316)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv319 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3583 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv317 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3589 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), (v : 'tv_kinded_variable_opt)), _, (defining_expr : 'tv_named)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3594 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3602 "flambda_parser.ml"
        ) = 
# 203 "flambda_parser.mly"
      ( let (var, kind) = v in
        Let { var; kind; defining_expr; body } )
# 3607 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)) : 'freshtv320)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv333 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3615 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv329 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3625 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv327 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3632 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s, (exn_and_stub : 'tv_exn_and_stub)), _, (name : 'tv_continuation)), _, (params : 'tv_typed_args)), _, (handler : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3637 "flambda_parser.ml"
            ))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _v : 'tv_continuation_handler = 
# 257 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 3645 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv325) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_continuation_handler) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState65 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv321 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | APPLY | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState169
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState169) : 'freshtv322)
            | MenhirState170 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv323 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | AND ->
                    _menhir_run170 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | APPLY | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
                    _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState171
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171) : 'freshtv324)
            | _ ->
                _menhir_fail ()) : 'freshtv326)) : 'freshtv328)) : 'freshtv330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv331 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3690 "flambda_parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv337 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3699 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv335 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3705 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (recursive : 'tv_recursive)), _, (handler : 'tv_continuation_handler)), _, (t : 'tv_andk)), _, (body : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3710 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3716 "flambda_parser.ml"
        ) = 
# 211 "flambda_parser.mly"
     ( let handlers = handler :: t in
       Let_cont { recursive; body; handlers } )
# 3721 "flambda_parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv336)) : 'freshtv338)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv347 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3729 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv345 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3735 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _menhir_s, (name : 'tv_func_sym)), _, (params : 'tv_typed_args)), _, (ret_cont : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (ret_arity : 'tv_return_arity)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3740 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _7 = () in
        let _3 = () in
        let _v : 'tv_let_code = 
# 126 "flambda_parser.mly"
  ( ({ name; params; ret_cont; ret_arity; exn_cont; expr } : let_code) )
# 3747 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv343) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_let_code) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv339 * _menhir_state)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((let_code : 'tv_let_code) : 'tv_let_code) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 114 "flambda_parser.mly"
                                          ( Let_code let_code )
# 3767 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)) : 'freshtv346)) : 'freshtv348)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv357 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_args) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3775 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv355 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_args) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3781 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, (c : 'tv_continuation)), _, (exn_cont : 'tv_option_exn_continuation_)), _, (v : 'tv_args)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3786 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_effect = 
# 283 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = exn_cont; computed_values = v }
        in
        { computation = Some computation; static_structure = [] } )
# 3795 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv353) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_effect) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv349 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((e : 'tv_effect) : 'tv_effect) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 102 "flambda_parser.mly"
                                          ( Define_symbol (Nonrecursive, e) )
# 3814 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv350)) : 'freshtv352)) : 'freshtv354)) : 'freshtv356)) : 'freshtv358)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv361 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3822 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv359 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) * _menhir_state * (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3828 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s), _, (c : 'tv_continuation)), _, (v : 'tv_args)), _, (static : 'tv_nonempty_list_static_structure_)), _, (expr : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 3833 "flambda_parser.ml"
        ))) = _menhir_stack in
        let _6 = () in
        let _4 = () in
        let _1 = () in
        let _v : (
# 90 "flambda_parser.mly"
      (Fexpr.definition)
# 3841 "flambda_parser.ml"
        ) = 
# 272 "flambda_parser.mly"
      ( let computation =
          { expr; return_cont = c;
            exception_cont = None; computed_values = v }
        in
        { computation = Some computation; static_structure = static } )
# 3849 "flambda_parser.ml"
         in
        _menhir_goto_definition _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)
    | _ ->
        _menhir_fail ()

and _menhir_reduce71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_option_exn_continuation_ = 
# 114 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( None )
# 3860 "flambda_parser.ml"
     in
    _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_goto_recursive : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_recursive -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EXN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | STUB ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | LIDENT _ ->
            _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65) : 'freshtv282)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv283 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LIDENT _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv284)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv285 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | LETK | UIDENT _ ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState188
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188) : 'freshtv286)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_program_body_elt_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_program_body_elt_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState213 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv265 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state * 'tv_program_body_elt) * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_program_body_elt)), _, (xs : 'tv_list_program_body_elt_)) = _menhir_stack in
        let _v : 'tv_list_program_body_elt_ = 
# 213 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( x :: xs )
# 3939 "flambda_parser.ml"
         in
        _menhir_goto_list_program_body_elt_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv264)) : 'freshtv266)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv279 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv275 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv273 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (elts : 'tv_list_program_body_elt_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3958 "flambda_parser.ml"
            ) = 
# 98 "flambda_parser.mly"
                                       ( elts )
# 3962 "flambda_parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv271) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3970 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv269) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3978 "flambda_parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv267) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3986 "flambda_parser.ml"
            )) : (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 3990 "flambda_parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv268)) : 'freshtv270)) : 'freshtv272)) : 'freshtv274)) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv277 * _menhir_state * 'tv_list_program_body_elt_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | _ ->
        _menhir_fail ()

and _menhir_run2 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 83 "flambda_parser.mly"
       (string)
# 4006 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv261) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 83 "flambda_parser.mly"
       (string)
# 4017 "flambda_parser.ml"
    )) : (
# 83 "flambda_parser.mly"
       (string)
# 4021 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_symbol = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 355 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4029 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv259) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_symbol) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv227 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv225 * _menhir_state) * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (s : 'tv_symbol)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_program_body_elt = 
# 115 "flambda_parser.mly"
                                          ( Root s )
# 4047 "flambda_parser.ml"
         in
        _menhir_goto_program_body_elt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv226)) : 'freshtv228)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv235 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_func_sym = 
# 351 "flambda_parser.mly"
               ( s )
# 4059 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv231) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_func_sym) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv229 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | MINUSGREATER ->
            _menhir_reduce106 _menhir_env (Obj.magic _menhir_stack) MenhirState8
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv230)) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)
    | MenhirState209 | MenhirState185 | MenhirState29 | MenhirState173 | MenhirState73 | MenhirState165 | MenhirState163 | MenhirState81 | MenhirState154 | MenhirState148 | MenhirState145 | MenhirState142 | MenhirState105 | MenhirState134 | MenhirState132 | MenhirState118 | MenhirState86 | MenhirState85 | MenhirState79 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_simple = 
# 345 "flambda_parser.mly"
               ( Symbol s )
# 4088 "flambda_parser.ml"
         in
        _menhir_goto_simple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv238)) : 'freshtv240)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv243 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv241 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : 'tv_name = 
# 340 "flambda_parser.mly"
               ( (Symbol s:name) )
# 4100 "flambda_parser.ml"
         in
        _menhir_goto_name _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)) : 'freshtv244)
    | MenhirState189 | MenhirState206 | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv253 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv249 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BLOCK ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv245 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState196 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196) : 'freshtv246)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv247 * _menhir_state * 'tv_symbol)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv251 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)
    | MenhirState202 | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv257 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv255 * _menhir_state * 'tv_symbol) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (s : 'tv_symbol)) = _menhir_stack in
        let _v : (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4150 "flambda_parser.ml"
        ) = 
# 312 "flambda_parser.mly"
               ( Symbol s )
# 4154 "flambda_parser.ml"
         in
        _menhir_goto_of_kind_value _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)) : 'freshtv258)
    | _ ->
        _menhir_fail ()) : 'freshtv260)) : 'freshtv262)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 64 "flambda_parser.mly"
       (string)
# 4163 "flambda_parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv223) = Obj.magic _menhir_stack in
    let (_endpos_e_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((e : (
# 64 "flambda_parser.mly"
       (string)
# 4174 "flambda_parser.ml"
    )) : (
# 64 "flambda_parser.mly"
       (string)
# 4178 "flambda_parser.ml"
    )) = _v in
    let (_startpos_e_ : Lexing.position) = _startpos in
    ((let _v : 'tv_continuation = let _endpos = _endpos_e_ in
    let _startpos = _startpos_e_ in
    
# 372 "flambda_parser.mly"
               ( e, make_loc (_startpos, _endpos) )
# 4186 "flambda_parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv221) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_continuation) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv173 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | COLON | EQUAL ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState19
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv174)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv191 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (cont : 'tv_continuation)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_exn_continuation = 
# 119 "flambda_parser.mly"
                             ( cont )
# 4218 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exn_continuation) = _v in
        ((match _menhir_s with
        | MenhirState114 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv177 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv175 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((e : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let (((((_menhir_stack, _menhir_s), (func : 'tv_csymbol)), _, (args : 'tv_simple_args)), _, (ra : 'tv_return_arity)), _, (r : 'tv_continuation)) = _menhir_stack in
            let _7 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 4242 "flambda_parser.ml"
            ) = 
# 215 "flambda_parser.mly"
     ( Apply {
          func = Symbol func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = C_call {
              alloc = true; (* TODO noalloc *)
              (* param_arity =; *)
              return_arity = ra;
            };
       })
# 4256 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv176)) : 'freshtv178)
        | MenhirState129 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv181 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv179 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((e : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let ((((_menhir_stack, _menhir_s), _, (func : 'tv_name)), _, (args : 'tv_simple_args)), _, (r : 'tv_continuation)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (
# 92 "flambda_parser.mly"
      (Fexpr.expr)
# 4274 "flambda_parser.ml"
            ) = 
# 228 "flambda_parser.mly"
     ( Apply {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind = Function Indirect_unknown_arity;
       })
# 4284 "flambda_parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)
        | MenhirState188 | MenhirState178 | MenhirState19 | MenhirState102 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv185) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exn_continuation) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv183) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((x : 'tv_exn_continuation) : 'tv_exn_continuation) = _v in
            ((let _v : 'tv_option_exn_continuation_ = 
# 116 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( Some x )
# 4299 "flambda_parser.ml"
             in
            _menhir_goto_option_exn_continuation_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
        | _ ->
            _menhir_fail ()) : 'freshtv188)) : 'freshtv190)) : 'freshtv192)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_tag)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (i : 'tv_tag)), _, (c : 'tv_continuation)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_switch_case = 
# 177 "flambda_parser.mly"
                                          ( i,c )
# 4314 "flambda_parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv201) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_switch_case) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv199 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | INT _v ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | SEMICOLON ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | RBRACE ->
                _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack) MenhirState56
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56) : 'freshtv194)
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv195 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (c : 'tv_switch_case)) = _menhir_stack in
            let _v : 'tv_switch = 
# 182 "flambda_parser.mly"
                    ( [c] )
# 4349 "flambda_parser.ml"
             in
            _menhir_goto_switch _menhir_env _menhir_stack _menhir_s _v) : 'freshtv196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv197 * _menhir_state * 'tv_switch_case) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)) : 'freshtv200)) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LBRACE ->
            _menhir_reduce106 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71) : 'freshtv208)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | DEF | EFFECT | EOF | LET | RBRACE | ROOT ->
            _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv210)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv211 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | COLON | LBRACE ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv212)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv213 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState114
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv214)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv215 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState129
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv216)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | APPLY | BANG | BLOCK | CCALL | CLOSURE | CONT | FLOAT _ | HCF | INT _ | LET | LETK | LIDENT _ | LPAREN | OPAQUE | SWITCH | UIDENT _ | UNREACHABLE ->
            _menhir_reduce71 _menhir_env (Obj.magic _menhir_stack) MenhirState178
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv218)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv219 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAREN ->
            _menhir_run180 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | LBRACE ->
            _menhir_reduce4 _menhir_env (Obj.magic _menhir_stack) MenhirState191
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState191) : 'freshtv220)
    | _ ->
        _menhir_fail ()) : 'freshtv222)) : 'freshtv224)

and _menhir_reduce78 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_recursive = 
# 130 "flambda_parser.mly"
    ( Nonrecursive )
# 4461 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv171) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_recursive = 
# 131 "flambda_parser.mly"
        ( Recursive )
# 4475 "flambda_parser.ml"
     in
    _menhir_goto_recursive _menhir_env _menhir_stack _menhir_s _v) : 'freshtv172)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState213 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state * 'tv_program_body_elt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)
    | MenhirState209 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv9 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) * _menhir_state * 'tv_nonempty_list_static_structure_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)
    | MenhirState206 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv11 * _menhir_state * (
# 91 "flambda_parser.mly"
      (Fexpr.static_structure)
# 4497 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv12)
    | MenhirState202 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13 * _menhir_state * (
# 94 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4506 "flambda_parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)
    | MenhirState198 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv15 * _menhir_state * 'tv_symbol))) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)
    | MenhirState196 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv17 * _menhir_state * 'tv_symbol))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv19 * _menhir_state) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState191 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv21 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv25 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState188 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv27 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv31 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state * 'tv_kinded_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv35 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState179 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState173 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv43 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) * _menhir_state * 'tv_andk) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState171 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv45 * _menhir_state) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState169 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv49 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_continuation_handler) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState165 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv51 * _menhir_state) * 'tv_kinded_variable_opt)) * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState163 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv53 * _menhir_state) * 'tv_kinded_variable_opt)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState157 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state * 'tv_closure)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv57 * _menhir_state) * _menhir_state * 'tv_recursive) * _menhir_state * 'tv_separated_nonempty_list_AND_closure_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_named)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv61 * _menhir_state * 'tv_simple) * 'tv_infix_binop) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63 * _menhir_state * 'tv_simple))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65 * _menhir_state * 'tv_unop) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv69 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState128 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv71 * _menhir_state) * _menhir_state * 'tv_name) * _menhir_state * 'tv_simple_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state) * _menhir_state * 'tv_name) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv79 * _menhir_state) * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv83 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv85 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv87 * _menhir_state)) * 'tv_csymbol)) * _menhir_state * 'tv_simple_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv89 * _menhir_state)) * 'tv_csymbol)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState105 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv91 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv93 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv95 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv97 * _menhir_state * 'tv_variable) * _menhir_state * 'tv_typed_args) * 'tv_closure_elements)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv102)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * 'tv_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv109 * _menhir_state * 'tv_simple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv117 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv119 * _menhir_state)) * _menhir_state * 'tv_kinded_variable)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv121 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv123 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState71 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv125 * _menhir_state * 'tv_exn_and_stub) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_exn_and_stub) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState65 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state) * _menhir_state * 'tv_recursive) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_switch_case)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState53 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_tag)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv137 * _menhir_state) * 'tv_switch_sort) * _menhir_state * 'tv_simple)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * 'tv_switch_sort) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv141 * _menhir_state * 'tv_tag_to_size)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv143)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv144)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv145 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) * _menhir_state * 'tv_return_arity)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv147 * _menhir_state * 'tv_kind)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv149 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv151 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) * _menhir_state * 'tv_option_exn_continuation_) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv155 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) * _menhir_state * 'tv_continuation) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv157 * _menhir_state * 'tv_func_sym) * _menhir_state * 'tv_typed_args)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState12 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_typed_variable) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_func_sym) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv165 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState1 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv167 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv169) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv170)

and _menhir_reduce47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_program_body_elt_ = 
# 211 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
    ( [] )
# 4903 "flambda_parser.ml"
     in
    _menhir_goto_list_program_body_elt_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | UIDENT _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState1 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CODE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | UIDENT _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState5 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv4)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)

and _menhir_run176 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LIDENT _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176

and _menhir_run187 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | REC ->
        _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState187
    | LETK | STAR | UIDENT _ ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack) MenhirState187
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 89 "flambda_parser.mly"
      (Fexpr.program)
# 4989 "flambda_parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run187 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EFFECT ->
        _menhir_run176 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ROOT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 374 "flambda_parser.mly"
  

# 5026 "flambda_parser.ml"

# 269 "/home/chambart/.opam/build-flamdba/lib/menhir/standard.mly"
  

# 5031 "flambda_parser.ml"
