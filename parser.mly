%token FN
%token EOF
%token<string> ID
%token<string> NCONST
%token<string> SCONST
%token LPAREN RPAREN
%token LCPAREN RCPAREN
%token ARROW COLON COMMA NULL
%token LET EQUAL IN HANDLE BAR WITH ANY SET MUT EXCL SEMICOLON
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
| v = ID COLON t = MUT ID a = args
    { (v,"mut") :: a }
| COMMA a = args
    { a }
| { [] }

expr:
| LET bind_var = expr EQUAL bind_expr = expr IN body = expr
    { Let { bind_var; bind_expr; body } }
| v = ID SET bind_expr = expr SEMICOLON body = expr
    { Let { bind_var = Any ; bind_expr = Set {var = MutVar v; body = bind_expr} ; body } }
| bind_expr = expr SEMICOLON body = expr
    { Let { bind_var = Any ; bind_expr; body } }
| HANDLE body = expr WITH branches = pattern
    { Handle { body ; branches = Array.of_list branches } }
| f = ID LPAREN a = u_args RPAREN
    { FunApp {func_name = f; args = Array.of_list a } }
| e1 = expr PLUS e2 = expr
    { BinOp (e1,e2,"+") }
| v = ID SET e=expr
    { Set { var = MutVar v ; body = e} }
| MUT v = ID
    { MutVar v }
| EXCL v = ID
    { MutVar v }
| v = ID
    { Var v }
| n = NCONST
    { Const (n, TyInt) }
| s = SCONST
    { Const (s, TyString) }
| ANY
    { Any }
| NULL
    { Null }

u_args:
| e = expr a = u_args
    { e :: a }
| COMMA a = u_args { a }
| { [] }

pattern:
| BAR u = u_args ARROW e = expr p = pattern
  { (u,e) :: p }
| { [] }
