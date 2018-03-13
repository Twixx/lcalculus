{
        open Parser        (* The type token is defined in parser.mli *)
        exception Eof
        open Lexing
        open Parsing
        exception LexErr of string
        exception ParseErr of string

        let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum 
          (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

        let lex_error lexbuf = 
            raise ( LexErr (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

        let parse_error msg nterm =
            raise ( ParseErr (error msg (rhs_start_pos nterm) (rhs_end_pos nterm)))
}

rule token = parse
            [' ' '\t' '\n']     { token lexbuf }     (* skip blanks *)
          | "->"                { IS }
          | "by"                { BY }
          | "let"               { LET }
          | "in"                { IN }
          | "E-App1"            { RULENAME APP1 }
          | "E-App2"            { RULENAME APP2 }
          | "E-AppAbs"          { RULENAME APPABS }
          | "E-AppFull"         { RULENAME APPFULL }
          | "E-Let1"            { RULENAME LET1 }
          | "E-Let2"            { RULENAME LET2 }
          | "E-LetAbs"          { RULENAME LETABS }
          | ['a'-'z']+ as id    { ID id }
          | ';'                 { SEMI }
          | '='                 { EQ }
          | '{'                 { LBRA }
          | '}'                 { RBRA }
          | '\\'                { LAMBDA }
          | '.'                 { DOT }
          | '('                 { LPAREN }
          | ')'                 { RPAREN }
          | eof                 { raise Eof }
          | _                   { lex_error lexbuf }
