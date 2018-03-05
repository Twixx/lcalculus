open Dotgraph
open Checker

let _ =
    try
        (*Parsing.set_trace true;*)
        let lexbuf = Lexing.from_channel stdin in
        let result = Parser.expr Lexer.token lexbuf in
        (*print_graph result;*)
        check_deriv result
    with Lexer.Eof ->
        exit 0
