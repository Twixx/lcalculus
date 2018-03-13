open Ast

(* functions to check the number of premises *)
let check_premise1 rule exprs =
    match exprs with
    | [Expr(p, _, _, _)] -> p
    | _ -> raise (Premise_num_error(1, rule))

let check_premise0 rule exprs =
    if exprs <> [] then
        raise (Premise_num_error(0, rule))

let check_app2 judg premise =
    let Judgement(l, r, p) = judg in
    match l, r with
    | Application(Abstraction _ as v1, t2, _), Application(Abstraction _ as v1', t2', _)
    | Application(FreeId _ as v1, t2, _), Application(FreeId _ as v1', t2', _) ->
            let Judgement(j1, j1', p2) = premise in
            if term_ne v1 v1' || term_ne t2 j1 || term_ne t2' j1' then
                raise (Pattern_error(p, p2))
    | _, _ -> raise (Conclusion_error p)

let check_let2 judg premise =
    let Judgement(l, r, p) = judg in
    match l, r with
    | LetBind(Abstraction _ as a, t, _), LetBind(Abstraction _ as a', t', _)
    | LetBind(FreeId _ as a, t, _), LetBind(FreeId _ as a', t', _) ->
            let Judgement(j, j', p2) = premise in
            if term_ne a a' || term_ne t j || term_ne t' j' then
                raise (Pattern_error(p, p2))
    | _, _ -> raise (Conclusion_error p)

let check_app1 judg premise =
    let Judgement(l, r, p) = judg in
    match l, r with
    | Application(t1, t2, p), Application(t1', t2', _) ->
            let Judgement(j1, j1', p2) = premise in
                if term_ne t2 t2' || term_ne t1 j1 || term_ne t1' j1' then
                raise (Pattern_error(p, p2))
    | _, _ -> raise (Conclusion_error p)

let check_let1 judg premise =
    let Judgement(l, r, p) = judg in
    match l, r with
    | LetBind(a, t, p), LetBind(a', t', _) ->
            let Judgement(j, j', p2) = premise in
                if term_ne t t' || term_ne a j || term_ne a' j' then
                raise (Pattern_error(p, p2))
    | _, _ -> raise (Conclusion_error p)

let substitute term v =
    let rec substitute' term v n = 
    match term with
    | Application(t1, t2, p) -> Application(substitute' t1 v n, substitute' t2 v n, p)
    | Abstraction(t, p) -> Abstraction(substitute' t v (n+1), p)
    | BoundedId(id, _) when id = n -> v
    | _ as v -> v
    in substitute' term v 0

let check_appabs judg =
    let Judgement(l, r, p) = judg in
    match l, r with
    | Application(Abstraction(t1, _), (Abstraction _ as v2), _), t2
    | Application(Abstraction(t1, _), (FreeId _ as v2), _), t2
      when term_eq (substitute t1 v2) t2 -> ()
    | _, _ -> raise (Conclusion_error p)

let check_letabs judg =
    let Judgement(l, r, p) = judg in
    match l, r with
    | LetBind((Abstraction _ as a), t, _), t'
    | LetBind((FreeId _ as a), t, _), t'
      when term_eq (substitute t a) t' -> ()
    | _, _ -> raise (Conclusion_error p)

(* Lookup the ast and the term
 * Check that they are structurally equal
 * Check that all occurrences of the nameless representation
 * of the name bounded in ast match a free variable and are
 * not in conflict with the current candidate
 * *)

let bind_id abs term candidate =
    let rec bind_id' n abs term (candidate : freeid option) =
        match abs, term with
        | Abstraction(t, p), Abstraction(t', _) ->
                bind_id' (n+1) t t' candidate
        | Application(t1, t2, p), Application(t1', t2', _) ->
                let c1 = bind_id' n t1 t1' candidate in
                bind_id' n t2 t2' c1
        | BoundedId(id, p), FreeId(id2, p2) when id = n ->
                (match candidate with
                 | None -> Some(id2, p2)
                 | Some((idc, _)) when idc = id2 -> candidate
                 | Some((idc, pc)) -> raise (Candidate_error((id2, p2), (idc, pc))))
        | BoundedId(id, p), BoundedId(id2, _) when id = id2 ->
                candidate
        | FreeId(s, p), FreeId(s2, _) when s = s2 ->
                candidate
        | _, _ -> raise (Pattern_error(term_info abs, term_info term))
    in bind_id' 0 abs term candidate


let check_appfull judg premise =
    let Judgement(l, r, p) = judg in
    match l, r with
    | Abstraction(t1, _), Abstraction(t1', _) ->
            let Judgement(j1, j1', _) = premise in
            let c1 = bind_id t1 j1 None in
            let _ = bind_id t1' j1' c1 in ()
    | _, _ -> raise (Conclusion_error p)

let rec check_deriv ast =
    try
        match ast with
        | Expr(judg, (Rule(rule, _) as r), exprs, _) ->
                (match rule with
                | APP1 ->
                        check_app1 judg (check_premise1 r exprs)
                | APP2 ->
                        check_app2 judg (check_premise1 r exprs)
                | APPABS ->
                        check_premise0 r exprs;
                        check_appabs judg
                | APPFULL ->
                        check_appfull judg (check_premise1 r exprs)
                | LET1 ->
                        check_let1 judg (check_premise1 r exprs)
                | LET2 ->
                        check_let2 judg (check_premise1 r exprs)
                | LETABS ->
                        check_premise0 r exprs;
                        check_letabs judg);
                check_premises exprs
    with
    | Conclusion_error p -> 
        print_string
        ("The expression doesn't match the conclusion of the rule " ^ (string_of_info p) ^ "\n")
    | Premise_num_error(n, Rule(s, p)) ->
        print_string
        ("The number of premise(s) doesn't match the number needed for the rule " ^ (rule_name s) ^ " (" ^ (string_of_int n)
        ^ ") " ^ (string_of_info p) ^ "\n")
    | Pattern_error(p1, p2) ->
        print_string
        ("The conclusion " ^ (string_of_info p1) ^ " doesn't match the premise(s) " ^ (string_of_info p2) ^ "\n")
    | Candidate_error((s1, p1), (s2, p2)) ->
        print_string
        ("The name of the candidate (\"" ^ s1 ^ "\", " ^ (string_of_info p1) ^ ") for the bounded name of the expression " ^
        " doesn't match the (\"" ^ s2 ^ "\", " ^ (string_of_info p2) ^ ")\n")

    and check_premises exprs =
        match exprs with
    | [] -> ()
    | h::t -> check_deriv h; check_premises t

