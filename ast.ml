type rule = APP1 | APP2 | APPABS

type context = string list

type term = Id of string
          | Abstraction of string * term
          | Application of term * term

type judgement = Judgement of term * term

type expr = Expr of judgement * rule * expr list

exception Check_error of string

(*
let rec list_eq l1 l2 = 
    match l1, l2 with
    | h1 :: t1, h2 :: t2 -> ast_equal h1 h2 && list_eq t1, t2
    | [], [] -> true
    | _, _ -> false

let rec ast_equal ast1 ast2 =
    match ast1, ast2 with
    Expr(j1, r1, e1), Expr(j2, r2, e2) ->
        judg_equal j1 j2 && rule_equal r1 r2 && list_eq e1 e2

let rec judg_equal j1 j2 =
    match j1, j2 with
    Judgement(t1,t2), Judgement(t3,t4) ->
        term_equal t1 t3 && term_equal t2 t4

let rec term_equal t1 t2 =
    match t1, t2 with
    Id(s1), Id(s2) -> 
*)
