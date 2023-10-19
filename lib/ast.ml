type id = string
type binary_op = Add | Mult | And | Or | Lt | Gt | Eq | NotEq | Lte | Gte
type unary_op = Not
type loc = Lexing.position

type expr =
  | Var of loc * id
  | Int of loc * int
  | Bool of loc * bool
  | BinaryOp of loc * binary_op * expr * expr
  | UnaryOp of loc * unary_op * expr
  | Let of loc * id * expr * expr
  | If of loc * expr * expr * expr
