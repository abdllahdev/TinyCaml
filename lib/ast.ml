type id = string
type binary_op = Add | Mult | And | Or | Lt | Gt | Eq | NotEq | Lte | Gte
type unary_op = Not
type loc = Lexing.position
type value = VBool of bool | VInt of int

type expr =
  | ExprInt of loc * int
  | ExprBool of loc * bool
  | ExprVar of loc * id
  | ExprBinaryOp of loc * binary_op * expr * expr
  | ExprUnaryOp of loc * unary_op * expr
  | ExprLet of loc * id * expr * expr
  | ExprIf of loc * expr * expr * expr
