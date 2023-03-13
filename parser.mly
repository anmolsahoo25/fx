%token FN
%token EOF
%token<string> ID
%token<string> NCONST
%token<string> SCONST
%token LPAREN RPAREN
%token LCPAREN RCPAREN
%token ARROW COLON COMMA
%token LET EQUAL IN HANDLE BAR WITH PERFORM ANY
%token PLUS

%start <Ast.t> prog

%{
    open Ast
%}

%%

prog:
| EOF
    { { func_decls = [] } }
| FN s = ID LPAREN a = args RPAREN ARROW t = ID LCPAREN e = expr RCPAREN p = prog
    { { func_decls = { name = s; ret_type = t; args = Array.of_list a; body = e } :: p.func_decls } }

args:
| v = ID COLON t = ID a = args
    { (v,t) :: a }
| COMMA a = args { a }
| { [] }

expr:
| LET bind_var = expr EQUAL bind_expr = expr IN body = expr
    { Let { bind_var; bind_expr; body } }
| HANDLE body = expr WITH branches = pattern
    { Handle { body ; branches } }
| PERFORM e = expr
    { Perform { effect = e } }
| f = ID LPAREN a = u_args RPAREN
    { FunApp {func_name = f; args = Array.of_list a } }
| e1 = expr PLUS e2 = expr
    { BinOp (e1,e2,"+") }
| v = ID
    { Var v }
| n = NCONST
    { Const (n, TyInt) }
| s = SCONST
    { Const (s, TyString) }
| ANY
    { Any }

u_args:
| e = expr a = u_args
    { e :: a }
| COMMA a = u_args { a }
| { [] }

pattern:
| BAR s = expr ARROW t = expr p = pattern
  { (s,t) :: p }
| { [] }
