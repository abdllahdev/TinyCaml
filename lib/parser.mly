%{
open Ast
%}

%token <string> ID
%token <int> INT
%token LPAREN
%token RPAREN
%token NOT
%token AND
%token OR
%token LT
%token GT
%token EQ
%token NOT_EQ
%token LTE
%token GTE
%token PLUS
%token TIMES
%token EQUALS
%token TRUE
%token FALSE
%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN
%nonassoc ELSE

%right AND
%right OR

%left LT
%left GT
%left EQ
%left NOT_EQ
%left LTE
%left GTE
%left PLUS
%left TIMES

%start <Ast.expr> ast

%%

ast:
	| e = expr; EOF { e }
	;

expr:
	| i = INT { Int ($startpos, i) }
	| id = ID { Var ($startpos, id) }
	| TRUE { Bool ($startpos, true) }
	| FALSE { Bool ($startpos, false) }
	| NOT; e = expr; { UnaryOp ($startpos, Not, e) }
	| e1 = expr; AND; e2 = expr { BinaryOp ($startpos, And, e1, e2) }
	| e1 = expr; OR; e2 = expr { BinaryOp ($startpos, Or, e1, e2) }
	| e1 = expr; LT; e2 = expr { BinaryOp ($startpos, Lt, e1, e2) }
	| e1 = expr; GT; e2 = expr { BinaryOp ($startpos, Gt, e1, e2) }
	| e1 = expr; EQ; e2 = expr { BinaryOp ($startpos, Eq, e1, e2) }
	| e1 = expr; NOT_EQ; e2 = expr { BinaryOp ($startpos, NotEq, e1, e2) }
	| e1 = expr; LTE; e2 = expr { BinaryOp ($startpos, Lte, e1, e2) }
	| e1 = expr; GTE; e2 = expr { BinaryOp ($startpos, Gte, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { BinaryOp ($startpos, Mult, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { BinaryOp ($startpos, Add, e1, e2) }
	| LET; id = ID; EQUALS; e1 = expr; IN; e2 = expr { Let ($startpos, id, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If ($startpos, e1, e2, e3) }
	| LPAREN; e = expr; RPAREN { e } 
	;
