%token LPAREN RPAREN
%token LBRA RBRA
%token EOL
%token BY
%token RULEAPP1
%token RULEAPP2
%token RULEAPPABS
%token LBRA
%token RBRA
%token LAMBDA
%token DOT
%token LPAREN
%token RPAREN
%token IS
%token SEMI
%token <string> ID

%start expr
%type <Ast.expr> expr
%type <Ast.rule> rule
%type <Ast.judgement> judgement
%type <string list -> Ast.term> term

%%

expr:
    | judgement BY rule LBRA expr_list RBRA {Expr($1, $3, $5)}
    ;

expr_list:
      /*empty*/             {[]}
    | expr                  {[$1]}
    | expr_list SEMI expr   {$1@[$3]}
    ;

rule:
      RULEAPP1      {APP1}
    | RULEAPP2      {APP2}
    | RULEAPPABS    {APPABS}
    ;

judgement:
    term IS term {Judgement($1 [], $3 [])}
    ;

term:
      LAMBDA ID DOT term {
          fun ctx -> let new_ctx = $2::ctx in 
          Abstraction($4 new_ctx)
        }
    | app               {$1}
    ;

app:
    app id       {fun ctx -> Application($1 ctx, $2 ctx)}
    | id         {$1}
    ;

id:
      ID {
              let rec lookup n ctx = match ctx with
                  | [] -> Ast.FreeId($1)
                  | h::t -> if h = $1 then BoundedId(n)
                            else lookup (n+1) t
              in lookup 0
         }
    | LPAREN term RPAREN        {$2}
    ;
