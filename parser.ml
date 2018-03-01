type token =
  | LPAREN
  | RPAREN
  | LBRA
  | RBRA
  | EOL
  | BY
  | RULEAPP1
  | RULEAPP2
  | RULEAPPABS
  | LAMBDA
  | DOT
  | IS
  | SEMI
  | ID of (string)

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRA *);
  260 (* RBRA *);
  261 (* EOL *);
  262 (* BY *);
  263 (* RULEAPP1 *);
  264 (* RULEAPP2 *);
  265 (* RULEAPPABS *);
  266 (* LAMBDA *);
  267 (* DOT *);
  268 (* IS *);
  269 (* SEMI *);
    0|]

let yytransl_block = [|
  270 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\005\000\005\000\005\000\002\000\002\000\002\000\003\000\
\004\000\004\000\006\000\006\000\007\000\007\000\000\000"

let yylen = "\002\000\
\006\000\000\000\001\000\003\000\001\000\001\000\001\000\003\000\
\004\000\001\000\002\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\013\000\015\000\000\000\000\000\
\000\000\012\000\000\000\000\000\000\000\000\000\011\000\014\000\
\000\000\005\000\006\000\007\000\000\000\008\000\009\000\000\000\
\003\000\000\000\001\000\000\000\004\000"

let yydgoto = "\002\000\
\006\000\021\000\007\000\008\000\026\000\009\000\010\000"

let yysindex = "\003\000\
\002\255\000\000\002\255\006\255\000\000\000\000\015\255\011\255\
\001\255\000\000\022\255\014\255\254\254\002\255\000\000\000\000\
\002\255\000\000\000\000\000\000\023\255\000\000\000\000\002\255\
\000\000\253\254\000\000\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\007\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\250\255\000\000\000\000\253\255\000\000\000\000\018\000"

let yytablesize = 27
let yytable = "\011\000\
\027\000\003\000\003\000\001\000\018\000\019\000\020\000\002\000\
\010\000\028\000\022\000\004\000\010\000\023\000\005\000\005\000\
\002\000\025\000\010\000\012\000\013\000\029\000\014\000\016\000\
\017\000\024\000\015\000"

let yycheck = "\003\000\
\004\001\001\001\001\001\001\000\007\001\008\001\009\001\004\001\
\002\001\013\001\014\000\010\001\006\001\017\000\014\001\014\001\
\013\001\024\000\012\001\014\001\006\001\028\000\012\001\002\001\
\011\001\003\001\009\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRA\000\
  RBRA\000\
  EOL\000\
  BY\000\
  RULEAPP1\000\
  RULEAPP2\000\
  RULEAPPABS\000\
  LAMBDA\000\
  DOT\000\
  IS\000\
  SEMI\000\
  "

let yynames_block = "\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Ast.judgement) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.rule) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 27 "parser.mly"
                                            (Expr(_1, _3, _5))
# 114 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 31 "parser.mly"
                            ([])
# 120 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 32 "parser.mly"
                            ([_1])
# 127 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 33 "parser.mly"
                            (_1@[_3])
# 135 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                    (APP1)
# 141 "parser.ml"
               : Ast.rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                    (APP2)
# 147 "parser.ml"
               : Ast.rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                    (APPABS)
# 153 "parser.ml"
               : Ast.rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 43 "parser.mly"
                 (Judgement(_1, _3))
# 161 "parser.ml"
               : Ast.judgement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.term) in
    Obj.repr(
# 47 "parser.mly"
                         (Abstraction(_2, _4))
# 169 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app) in
    Obj.repr(
# 48 "parser.mly"
                        (_1)
# 176 "parser.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 52 "parser.mly"
                    (Application(_1, _2))
# 184 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 53 "parser.mly"
                    (_1)
# 191 "parser.ml"
               : 'app))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
                                (Id(_1))
# 198 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.term) in
    Obj.repr(
# 58 "parser.mly"
                                (_2)
# 205 "parser.ml"
               : 'value))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
