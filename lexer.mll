{
        open Parser        (* The type token is defined in parser.mli *)
        open Lexing
        open Parsing
        exception Eof
        exception LexErr of string
        exception ParseErr of string

        let newline lexbuf =
            let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
                { pos with pos_lnum = pos.pos_lnum + 1;
                   pos_bol = pos.pos_cnum }
                
        let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum 
          (start.pos_cnum -start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

        let lex_error lexbuf = 
            raise ( LexErr (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

        let parse_error msg nterm =
            raise ( ParseErr (error msg (rhs_start_pos nterm) (rhs_end_pos nterm)))

        let comment_depth = ref 0
}

rule token = parse
            [' ' '\t']          { token lexbuf }     (* skip blanks *)
          | "\n"                { newline lexbuf; token lexbuf}
          | "(*"                { comment_depth := 1; comment lexbuf }
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

and comment = parse
           "*)"                 { decr comment_depth;
                                  if !comment_depth = 0 then token lexbuf
                                  else comment lexbuf }

          | "(*"               { incr comment_depth; comment lexbuf}
          | "\n"               { newline lexbuf; comment lexbuf}
          | eof                { lex_error lexbuf }
          | _                  { comment lexbuf }

