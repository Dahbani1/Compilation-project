
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | SUB
    | PUSH of (
# 13 "pfx/basic/parser.mly"
       (int)
# 15 "pfx/basic/parser.ml"
  )
    | POP
    | MUL
    | MOD
    | INT of (
# 12 "pfx/basic/parser.mly"
       (int)
# 23 "pfx/basic/parser.ml"
  )
    | EOF
    | DIV
    | ADD
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState12
  | MenhirState1

# 1 "pfx/basic/parser.mly"
  
 open Ast


# 49 "pfx/basic/parser.ml"

let rec _menhir_goto_command_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.command list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, (_1 : (
# 12 "pfx/basic/parser.mly"
       (int)
# 66 "pfx/basic/parser.ml"
            ))), _, (_2 : (Ast.command list))) = _menhir_stack in
            let _v : (Ast.program) = 
# 37 "pfx/basic/parser.mly"
                         ( _1, _2 )
# 71 "pfx/basic/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.program)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Ast.command))), _, (_2 : (Ast.command list))) = _menhir_stack in
        let _v : (Ast.command list) = 
# 42 "pfx/basic/parser.mly"
                         ( _1 :: _2 )
# 90 "pfx/basic/parser.ml"
         in
        _menhir_goto_command_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_command : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.command) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADD ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DIV ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MOD ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MUL ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | POP ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PUSH _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | SUB ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | EOF ->
        _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.command list) = 
# 41 "pfx/basic/parser.mly"
                ( [] )
# 138 "pfx/basic/parser.ml"
     in
    _menhir_goto_command_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 48 "pfx/basic/parser.mly"
             ( Operate Sub )
# 149 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "pfx/basic/parser.mly"
       (int)
# 156 "pfx/basic/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (
# 12 "pfx/basic/parser.mly"
       (int)
# 170 "pfx/basic/parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        let _v : (Ast.command) = 
# 45 "pfx/basic/parser.mly"
             ( Push _2 )
# 176 "pfx/basic/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 46 "pfx/basic/parser.mly"
             ( Pop )
# 193 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 49 "pfx/basic/parser.mly"
             ( Operate Mul )
# 204 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 51 "pfx/basic/parser.mly"
             ( Operate Mod )
# 215 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 50 "pfx/basic/parser.mly"
             ( Operate Div )
# 226 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.command) = 
# 47 "pfx/basic/parser.mly"
             ( Operate Add )
# 237 "pfx/basic/parser.ml"
     in
    _menhir_goto_command _menhir_env _menhir_stack _menhir_s _v

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

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | DIV ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | MOD ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | MUL ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | POP ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | PUSH _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
        | SUB ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | EOF ->
            _menhir_reduce8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)

# 52 "pfx/basic/parser.mly"
  

# 300 "pfx/basic/parser.ml"

# 269 "<standard.mly>"
  

# 305 "pfx/basic/parser.ml"
