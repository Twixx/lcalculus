open Ast
let idc = ref 0

let print_node label p_id =
    idc := !idc + 1;
    print_string "N_";
    print_int !idc;
    print_string " [label=\"";
    print_string label;
    print_string "\" ];\nN_";
    print_int p_id;
    print_string " -- N_";
    print_int !idc;
    print_string ";\n";
    !idc

        
let rec print_term p_id term =
    match term with
        | Id(s) ->
            let _ = print_node ("id : " ^ s) p_id in ()
        | Abstraction(s, t1) ->
            let current = print_node ("abs : " ^ s) p_id in
            print_term current t1
        | Application(t1,t2) ->
            let current = print_node "app" p_id in
            print_term current t1;
            print_term current t2
            
let print_judg p_id judg =
    match judg with
    Judgement(t1,t2) ->
        let current = print_node "judg" p_id in
        print_term current t1;
        print_term current t2

let rec print_tree p_id expr =
    match expr with 
    Expr(judg,_,exprs) ->
        let current = print_node "expr" p_id in
        print_judg current judg;
        List.iter (print_tree current) exprs


let check_app2 judg exprs =
    let expr = match exprs with
    | [e] -> e
    | _ -> raise (Check_error "App2: more than one premise") in

    let premise = match expr with Expr(j,_,_) -> j in
    let l, r = match judg with Judgement(l,r) -> l,r in
    match l, r with
    | Application(Abstraction _ as v1, t2), Application(Abstraction _ as v1', t2') ->
            (match premise with Judgement(j1, j1') ->
            v1 = v1' && t2 = j1 && t2' = j1')
    | _, _ -> raise (Check_error "App2 can't be applied")

let check_app1 judg exprs =
    let expr = match exprs with
    | [e] -> e
    | _ -> raise (Check_error "App1: more than one premise") in

    let premise = match expr with Expr(j,_,_) -> j in
    let l, r = match judg with Judgement(l,r) -> l,r in
    match l, r with
    | Application(t1, t2), Application(t1', t2') ->
            (match premise with Judgement(j1, j1') ->
            t2 = t2' && t1 = j1 && t1' = j1')
    | _, _ -> raise (Check_error "App1 can't be applied")


let check_appabs judg exprs = true (*
    (if List != [] then
        raise (Check_error "Abstraction premise not empty"));

    let l, r = match judg with Judgement(l,r) -> l,r in
    match l, r with
    | Application(Abstraction(x, t12), Abstraction _ as v2), t12 ->
            
    | _ -> raise (Check_error "AbsAbs can't be applied")
*)
let rec check_deriv ast =
    match ast with
    Expr(judg, rule, exprs) ->
        (match rule with
        | APP1 ->
            check_app1 judg exprs
        | APP2 ->
            check_app2 judg exprs
        | APPABS ->
            check_appabs judg exprs)
        && check_premises exprs

and check_premises exprs =
    match exprs with
    | [] -> true
    | h::t -> check_deriv h && check_premises t

let _ =
    try
        (*Parsing.set_trace true;*)
        let lexbuf = Lexing.from_channel stdin in
        let result = Parser.expr Lexer.token lexbuf in
        print_string "graph ast {\n";
        let r = print_node "root" 0 in
        print_tree r result;
        print_string "}";
        flush stdout;
        (*check_deriv result*)
    with Lexer.Eof ->
        exit 0
