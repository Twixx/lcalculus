
type context = string list

type rule_name = APP1 | APP2 | APPABS | APPFULL
type rule = Rule of rule_name * context

type term = FreeId of string
          | BoundedId of int
          | Abstraction of term
          | Application of term * term

type judgement = Judgement of term * term

type expr = Expr of judgement * rule * expr list

exception Check_error of string
