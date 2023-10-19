open Formatter

exception SyntaxError of string

let raise_syntax_error loc value =
  raise (SyntaxError (Fmt.str "@(%s): '%s'" (string_of_loc loc) value))
