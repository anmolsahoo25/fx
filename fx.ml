let compile file_name bin_dir =
  let bin_name = String.sub file_name 0 (String.length file_name - 3) in

  (* log information *)
  Printf.printf "[LOG]: Input file %s\n" file_name;
  Printf.printf "[LOG]: Output dir %s\n" bin_dir;
  Printf.printf "[LOG]: Output bin %s\n" bin_name;

  (* parse and compile file *)
  let lexbuf = Lexing.from_channel (open_in file_name) in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Printf.printf "[LOG]: AST\n%s\n" (Ast.show prog);
    Compiler.compile prog bin_name bin_dir
  with
  | Parser.Error -> Printf.printf "Parsing error\n"
  | _ ->
      Printf.printf "Unknown token line: %d, char: %d\n"
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)

let () =
  try
    let file_name = Sys.argv.(1) in
    let bin_dir = Sys.argv.(2) in
    compile file_name bin_dir
  with Invalid_argument _ -> Printf.printf "Invalid arguments"
