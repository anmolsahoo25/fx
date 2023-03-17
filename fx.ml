let compile file_name bin_dir =
  let bin_name = String.sub file_name 0 (String.length file_name - 3) in

  (* parse and compile file *)
  let lexbuf = Lexing.from_channel (open_in file_name) in
  try
    let prog = Parser.prog Lexer.token lexbuf in
    Compiler.compile prog bin_name bin_dir
  with
  | Parser.Error
  | Lexer.Lexing_error _ ->
      Printf.printf "Parse / Lex error\n";
      let start = Lexing.lexeme_start lexbuf in
      let _end = Lexing.lexeme_end lexbuf in
      let new_handle = (open_in file_name) in
      for _ = 0 to (start+1) do
          Printf.printf "%c" (input_char new_handle)
      done;
      Printf.printf " <--- Error here\n"
  | s ->
      Printf.printf "Compiler error: %s\n" (Printexc.to_string s)

let () =
  try
    let file_name = Sys.argv.(1) in
    let bin_dir = Sys.argv.(2) in
    compile file_name bin_dir
  with Invalid_argument _ -> Printf.printf "Invalid arguments"
