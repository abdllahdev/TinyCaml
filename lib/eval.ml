open Core
open Ast
open Errors
module Environment = Map.Make_tree (String)

type env = value Environment.t

let empty_env = Environment.empty

let rec eval (env : env) (expr : expr) : value =
  match expr with
  | ExprInt (_, i) -> VInt i
  | ExprBool (_, b) -> VBool b
  | ExprVar (loc, id) -> eval_var env loc id
  | ExprBinaryOp (loc, op, expr1, expr2) ->
      eval_binary_op env loc op expr1 expr2
  | ExprUnaryOp (loc, op, expr) -> eval_unary_op env loc op expr
  | ExprLet (loc, id, expr1, expr2) -> eval_let env loc id expr1 expr2
  | ExprIf (loc, guard, expr1, expr2) -> eval_if env loc guard expr1 expr2

and eval_var (env : env) (loc : loc) (id : id) : value =
  try Environment.find_exn env id
  with Not_found_s _ -> raise_unbound_variable_error loc id

and eval_binary_op (env : env) (loc : loc) (op : binary_op) (expr1 : expr)
    (expr2 : expr) : value =
  match (op, eval env expr1, eval env expr2) with
  | Add, VInt a, VInt b -> VInt (a + b)
  | Mult, VInt a, VInt b -> VInt (a * b)
  | And, VBool a, VBool b -> VBool (a && b)
  | Or, VBool a, VBool b -> VBool (a || b)
  | Lt, VInt a, VInt b -> VBool (a < b)
  | Gt, VInt a, VInt b -> VBool (a > b)
  | Eq, VInt a, VInt b -> VBool (a = b)
  | NotEq, VInt a, VInt b -> VBool (not (equal_int a b))
  | Lte, VInt a, VInt b -> VBool (a <= b)
  | Gte, VInt a, VInt b -> VBool (a >= b)
  | _ -> raise_type_error loc

and eval_unary_op (env : env) (loc : loc) (op : unary_op) (expr : expr) : value
    =
  match (op, eval env expr) with
  | Not, VBool b -> VBool (not b)
  | _ -> raise_type_error loc

and eval_let (env : env) (loc : loc) (id : id) (expr1 : expr) (expr2 : expr) :
    value =
  let v = eval env expr1 in
  match Environment.add env ~key:id ~data:v with
  | `Duplicate -> raise_type_error loc
  | `Ok env' -> eval env' expr2

and eval_if (env : env) (loc : loc) (guard : expr) (expr1 : expr) (expr2 : expr)
    : value =
  match eval env guard with
  | VBool true -> eval env expr1
  | VBool false -> eval env expr2
  | _ -> raise_type_error loc
