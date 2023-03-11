open Llvm
open Ast

let compile prog bin_name bin_dir =
  (* compile ast to llvm *)
  let context = create_context () in
  let prog_module = create_module context bin_name in
  let prog_ll_file = Printf.sprintf "%s/%s.ll" bin_dir bin_name in
  set_target_triple "arm64-apple-macosx13.0.0" prog_module;

  (* global variables *)
  let print =
    declare_function "printf"
      (function_type (i32_type context) [| pointer_type (i8_type context) |])
      prog_module
  in

  let hello_string =
      define_global "hello" (const_string context "hello") prog_module in

  (* compile functions *)
  let compile_func func prog_module context =
    let func_type = function_type (void_type context) [||] in
    let func_def = define_function func.name func_type prog_module in
    let entry_bb = entry_block func_def in
    let ibuilder = builder_at_end context entry_bb in
    let str_gep = build_gep hello_string [| (const_int
    (i64_type context) 0) ; (const_int (i64_type context) 0) |] "1" ibuilder
    in
    let _ = build_call print [| str_gep |] "2" ibuilder in
    let _ = build_call print [| str_gep |] "3" ibuilder in
    let _ = build_ret_void ibuilder in
    ()
  in
  (* compile the program *)
  List.iter (fun func -> compile_func func prog_module context) prog.func_decls;

  (* dump program to llvm ir *)
  print_module prog_ll_file prog_module;

  (* link runtime and make executable *)
  ignore
    (Sys.command
       (Printf.sprintf "clang %s ../runtime.c -o %s/%s" prog_ll_file bin_dir
          bin_name))
