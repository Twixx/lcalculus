%token LPAREN RPAREN
%token LBRA RBRA
%token BY
%token LAMBDA
%token DOT
%token IS
%token SEMI
%token <string> ID
%token <Ast.rule_name> RULENAME

%start expr
%type <Ast.expr> expr
%type <Ast.rule> rule
%type <Ast.judgement> judgement
%type <Ast.context -> Ast.term> term

%%

expr:
    e1 = judgement BY e2 = rule LBRA e3 = expr_list RBRA
    { Expr(e1, e2, e3) }
    ;

expr_list:
    (*empty*)
    { [] }
    | e = expr
    { [e] }
    | exprs = expr_list SEMI expr = expr
    { exprs@[expr] }
    ;

rule:
    name = RULENAME LPAREN ctx = ID+ RPAREN
    { Rule(name, ctx) }
    | name = RULENAME
    { Rule(name, []) }
    ;

judgement:
    e1 = term IS e2 = term
    { Judgement(e1 [], e2 []) }
    ;

term:
    LAMBDA id = ID DOT t = term
    { 
          fun ctx -> let new_ctx = id::ctx in 
          Abstraction(t new_ctx)
     }
    | e = app
    { e }
    ;

app:
    e1 = app e2 = id
    { 
        fun ctx -> Application(e1 ctx, e2 ctx)
     }
    | e = id
    { e }
    ;

id:
    id = ID
    { 
        let rec lookup n ctx =
            match ctx with
            | [] -> Ast.FreeId(id)
            | h::t -> if h = id then BoundedId(n)
                      else lookup (n+1) t
        in lookup 0
    }
    | LPAREN e = term RPAREN
    { e }
    ;
