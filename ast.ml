type t = { func_decls : func_decl list }

and func_decl = {
  name : string;
  ret_type : string;
  args : (string * string) list;
  body : expr;
}

and expr =
  | Any
  | Const of string
  | Var of string
  | Tuple of { elts : expr list }
  | FunApp of { func_name : string; args : expr list }
  | Let of { bind_var : expr ; bind_expr : expr; body : expr }
  | Handle of { body : expr; branches : (expr * expr) list }
  | Perform of { effect: expr }
[@@deriving show]
