type t = { func_decls : func_decl list }
and ty = TyInt | TyString | TyUnit

and func_decl = {
  name : string;
  ret_type : string;
  args : (string * string) array;
  body : expr;
}

and expr =
  | Any
  | Const of (string * ty)
  | Var of string
  | MutVar of string
  | BinOp of (expr * expr * string)
  | FunApp of { func_name : string; args : expr array }
  | Let of { bind_var : expr; bind_expr : expr; body : expr }
  | Set of { var : expr ; body : expr }
  | Handle of { body : expr; branches : (expr list * expr) array}
  | Perform of { effect : expr }
[@@deriving show]
