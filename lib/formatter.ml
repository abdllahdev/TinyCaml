open Ast

let string_of_loc loc =
  Fmt.str "File:%s, Line:%d, Position:%d" loc.Lexing.pos_fname
    loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

let string_of_binary_op binary_op =
  match binary_op with
  | Add -> "add"
  | Mult -> "mult"
  | And -> "and"
  | Or -> "or"
  | Lt -> "lt"
  | Gt -> "gt"
  | Eq -> "eq"
  | NotEq -> "not_eq"
  | Lte -> "lte"
  | Gte -> "gte"

let rec string_of_expr ~indent ast =
  let pad n = String.make (indent * n) ' ' in
  match ast with
  | ExprInt (_, i) -> Fmt.str "int(%d)" i
  | ExprBool (_, b) -> Fmt.str "bool(%b)" b
  | ExprVar (_, id) -> Fmt.str "var(%s)" id
  | ExprBinaryOp (_, op, e1, e2) ->
      Fmt.str "%s(\n%s%s,%s)" (string_of_binary_op op) (pad 2)
        (string_of_expr ~indent:(indent + 1) e1)
        (string_of_expr ~indent:(indent + 1) e2)
  | ExprUnaryOp (_, op, e) -> (
      match op with
      | Not ->
          Fmt.str "%snot(%s\n%s\n)" (pad 1) (pad 1) (string_of_expr ~indent e))
  | ExprLet (_, id, e1, e2) ->
      Fmt.str "%slet %s = %s in\n%s" (pad 1) id
        (string_of_expr ~indent:(indent + 2) e1)
        (string_of_expr ~indent:(indent + 2) e2)
  | ExprIf (_, e1, e2, e3) ->
      Fmt.str "%sif\n%s%s\n%sthen\n%s%s\n%selse\n%s%s" (pad 1) (pad 2)
        (string_of_expr ~indent:(indent + 2) e1)
        (pad 1) (pad 2)
        (string_of_expr ~indent:(indent + 2) e2)
        (pad 1) (pad 2)
        (string_of_expr ~indent:(indent + 2) e3)

let string_of_value value =
  match value with VBool b -> Fmt.str "%b" b | VInt i -> Fmt.str "%d" i
