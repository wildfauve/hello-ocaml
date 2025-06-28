%token <int> INT
%token PLUS
%token MULT
%token LPEREN
%token RPEREN
%token EOF

%left PLUS
%left MULT

%start prog
%type <Ast.expr> prog

%%

prog: 
    | expr; EOF { $1 }
    ;

expr:
    | INT { Int $1 }
    | expr; PLUS; expr { Binop (Add, $1, $3) }
    | expr; MULT; expr { Binop (Mult, $1, $3) }
    | LPEREN; expr; RPEREN { $2 }
    ;