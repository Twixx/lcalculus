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
%type <Ast.term> term

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
    term IS term {Judgement($1, $3)}
    ;

term:
      LAMBDA ID DOT term {Abstraction($2, $4)}
    | app               {$1}
    ;

app:
    app value       {Application($1, $2)}
    | value         {$1}
    ;

value:
      ID                        {Id($1)}
    | LPAREN term RPAREN        {$2}



