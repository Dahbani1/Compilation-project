%{
  open Ast
  open BinOp
%}

%token EOF PLUS MINUS TIMES DIV MOD LPAR RPAR
%token <int> INT
%token <string> IDENT

%start < Ast.expression > expression

%left PLUS MINUS
%left TIMES DIV MOD
%right UMINUS

%%

expression:
 | e=expr EOF            { e }

expr:
  | MINUS e=expr %prec UMINUS  { Uminus e }
  | e1=expr o=bop e2=expr      { Binop(o,e1,e2) }
  | e=simple_expr              { e }

simple_expr:
  | LPAR e=expr RPAR           { e }
  | id=IDENT                   { Var id }
  | i=INT                      { Const i }

%inline bop:
  | MINUS     { Bsub }
  | PLUS      { Badd }
  | TIMES     { Bmul }
  | DIV       { Bdiv }
  | MOD       { Bmod }

%%
