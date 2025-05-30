open Sys
open Core
open Cmdliner
open TinyCaml

let parse_with_error lexbuf =
  try Ok (Parser.ast TinyCaml.Lexer.read_token lexbuf)
  with Parser.Error ->
    Errors.raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf)

let parse_file filename =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let input_file =
  let doc = "Input file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT_FILE" ~doc)

let main input_file =
  let input_file = Fmt.str "%s/%s" (getcwd ()) input_file in
  Fmt.str "Interpreting %s\n" input_file |> print_string;
  match parse_file input_file with
  | Ok ast ->
      Eval.eval Eval.empty_env ast |> Eval.string_of_value |> print_endline
  | Error error -> Core.Error.to_string_hum error |> print_endline

let cmd =
  let doc = "TinyCaml" in
  let man = [ `S "DESCRIPTION"; `P "TinyCaml is just a subset of OCaml" ] in
  Cmd.v
    (Cmd.info "TinyCaml" ~version:"1.0" ~doc ~man)
    Term.(const main $ input_file)

let () = exit (Cmd.eval cmd)
