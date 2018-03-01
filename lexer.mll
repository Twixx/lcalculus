{
        open Parser        (* The type token is defined in parser.mli *)
        open Lexing
        open Parsing
        exception Eof
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
          | ';'                 { SEMI }
          | "E-App1"            { RULEAPP1 }
          | "E-App2"            { RULEAPP2 }
          | "E-AppAbs"          { RULEAPPABS }
          | ['a'-'z']+ as id    { ID id }
          | '{'                 { LBRA }
          | '}'                 { RBRA }
          | '\\'                { LAMBDA }
          | '.'                 { DOT }
          | '('                 { LPAREN }
          | ')'                 { RPAREN }
          | eof                 { raise Eof }
          | _                   { lex_error lexbuf }
