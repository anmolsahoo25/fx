%token FN
%token EOF
%token<string> ID
%token<string> NCONST
%token LPAREN RPAREN
%token LCPAREN RCPAREN
%token ARROW COLON COMMA
%token LET EQUAL IN HANDLE BAR WITH PERFORM ANY

%start <Ast.t> prog

%{
    open Ast
%}

%%

prog:
| EOF
    { { func_decls = [] } }
| FN s = ID LPAREN a = args RPAREN ARROW t = ID LCPAREN e = expr RCPAREN p = prog
    { { func_decls = { name = s; ret_type = t; args = a; body = e } :: p.func_decls } }

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
    { FunApp {func_name = f; args = a } }
| LPAREN u = u_args RPAREN
    { Tuple {elts = u} }
| v = ID
    { Var v }
| n = NCONST
    { Const n }
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
