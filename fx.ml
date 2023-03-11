let compile file_name =
  let lexbuf = Lexing.from_channel (open_in file_name) in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Printf.printf "AST:\n %s\n" (Ast.show prog)
  with
  | Parser.Error -> Printf.printf "Parsing error\n"
  | _ ->
      Printf.printf "Unknown token line: %d, char: %d\n"
        (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf)

let () =
  Printf.printf "Running fx compiler\n";
  try
    let file_name = Sys.argv.(1) in
    compile file_name
  with Invalid_argument _ -> Printf.printf "No file provided for compilation"
