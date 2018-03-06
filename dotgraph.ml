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
        | FreeId(s,_) ->
            let _ = print_node ("free id : " ^ s) p_id in ()
        | BoundedId(int_id,_) ->
            let _ = print_node ("bounded id : " ^ (string_of_int int_id)) p_id in ()
        | Abstraction(t1,_) ->
            let current = print_node ("abs") p_id in
            print_term current t1
        | Application(t1,t2,_) ->
            let current = print_node "app" p_id in
            print_term current t1;
            print_term current t2
            
let print_judg p_id judg =
    match judg with
    Judgement(t1,t2,_) ->
        let current = print_node "judg" p_id in
        print_term current t1;
        print_term current t2

let rec print_tree p_id expr =
    match expr with 
    Expr(judg,_,exprs,_) ->
        let current = print_node "expr" p_id in
        print_judg current judg;
        List.iter (print_tree current) exprs

let print_graph ast =
    print_string "graph ast {\n N_0 [label=\"root\"];\n";
    print_tree 0 ast;
    print_string "}";
    flush stdout

