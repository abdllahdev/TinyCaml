open Ast

exception SyntaxError of string
exception UnboundVariableError of string
exception TypeError of string

let raise_syntax_error loc value =
  raise (SyntaxError (Fmt.str "@(%s): `%s`" (string_of_loc loc) value))

let raise_unbound_variable_error loc id =
  raise
    (UnboundVariableError
       (Fmt.str "@(%s): variable `%s` is unbound" (string_of_loc loc) id))

let raise_type_error loc =
  raise
    (TypeError
       (Fmt.str "@(%s): this expression has type mismatch" (string_of_loc loc)))
