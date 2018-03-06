open Lexing

type context = string list
type info = position

type rule_name = APP1 | APP2 | APPABS | APPFULL
type rule = Rule of rule_name * info

type freeid = string * info
type term = FreeId of freeid
          | BoundedId of int * info
          | Abstraction of term * info
          | Application of term * term * info

type judgement = Judgement of term * term * info

type expr = Expr of judgement * rule * expr list * info

exception Conclusion_error of info
exception Premise_num_error of int * rule
exception Pattern_error of info * info
exception Candidate_error of freeid * freeid

let rec term_eq t1 t2 =
    match t1, t2 with
    | FreeId(s1, _), FreeId(s2, _) ->
            s1 = s2
    | BoundedId(i1, _), BoundedId(i2, _) ->
            i1 = i2
    | Abstraction(a1, _), Abstraction(a2, _) ->
            term_eq a1 a2
    | Application(a1, a2, _), Application(a1', a2', _) ->
            term_eq a1' a1' && term_eq a2 a2'
    | _, _ -> false

let term_ne t1 t2 =
    not (term_eq t1 t2)

let rule_name = function
    | APP1 -> "E-App1"
    | APP2 -> "E-App2"
    | APPABS -> "E-AppAbs"
    | APPFULL -> "E-AppFull"

let term_info = function
    | FreeId(_,i)
    | BoundedId(_,i)
    | Abstraction(_,i)
    | Application(_,_,i) -> i

let string_of_info info =
    "at line: " ^ (string_of_int info.pos_lnum)
    ^ ", col: " ^ (string_of_int info.pos_cnum)

