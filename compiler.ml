open Llvm
open Llvm_analysis
open Ast

(* compile ast to llvm *)
let compile prog bin_name bin_dir =
  let context = create_context () in
  let prog_module = create_module context bin_name in
  let prog_ll_file = Printf.sprintf "%s/%s.ll" bin_dir bin_name in
  set_target_triple "arm64-apple-macosx13.0.0" prog_module;

  (* map of functions declared *)
  let func_table = Hashtbl.create 50 in
  let var_counter = ref 0 in
  let global_counter = ref 0 in

  (* global declarations *)
  let print =
    declare_function "printf"
      (function_type (i32_type context) [| pointer_type (i8_type context) |])
      prog_module
  in

  Hashtbl.add func_table "print" print;

  (* llvm types *)
  let void_type = void_type context in
  let int_type = i64_type context in
  let str_type = i8_type context |> pointer_type in

  (* util functions *)
  let ret_type = function
    | "string" -> str_type
    | "int" -> int_type
    | "unit" -> void_type
    | _ -> failwith "Invalid return type"
  in

  (* compile function *)
  let get_new_var () =
    let c = !var_counter in
    var_counter := !var_counter + 1;
    Printf.sprintf "V%d" c
  in

  let get_new_global () =
    let c = !global_counter in
    global_counter := !global_counter + 1;
    Printf.sprintf "G%d" c
  in

  let rec compile_body ibuilder params = function
    | Const (n, TyInt) -> const_int int_type (int_of_string n)
    | Const (s, TyString) ->
        let global_name = get_new_global () in
        let global_value =
          define_global global_name
            (const_stringz context (s ^ "\n"))
            prog_module
        in
        build_gep global_value
          [| const_int int_type 0; const_int int_type 0 |]
          (get_new_var ()) ibuilder
    | Var v -> Array.find_opt (fun (s, _) -> s = v) params |> Option.get |> snd
    | BinOp (e1, e2, name) -> (
        match name with
        | "+" ->
            build_add
              (compile_body ibuilder params e1)
              (compile_body ibuilder params e2)
              (get_new_var ()) ibuilder
        | _ -> failwith "invalid op")
    | FunApp { func_name; args } ->
        let fun_call =
          build_call
            (Hashtbl.find func_table func_name)
            (Array.map (fun s -> compile_body ibuilder params s) args)
            (get_new_var ()) ibuilder
        in
        fun_call
    | Let { bind_var; bind_expr; body } -> (
        match bind_var with
        | Any ->
            let _ = compile_body ibuilder params bind_expr in
            compile_body ibuilder params body
        | Var v ->
            let bind_val = compile_body ibuilder params bind_expr in
            compile_body ibuilder
              (Array.concat [ [| (v, bind_val) |]; params ])
              body
        | _ -> failwith "binding a non var or wildcard")
    | _ -> failwith "Invalid parse value"
  in
  let compile_func func =
    let func_type =
      function_type (ret_type func.ret_type)
        (Array.map (fun (_, s) -> ret_type s) func.args)
    in
    let func_def = define_function func.name func_type prog_module in
    let entry_bb = entry_block func_def in
    let ibuilder = builder_at_end context entry_bb in

    (* add function to declarations *)
    Hashtbl.add func_table func.name func_def;

    (* compile the body *)
    let func_params = params func_def in
    Array.iteri (fun i (s, _) -> set_value_name s func_params.(i)) func.args;
    let named_params =
      Array.mapi (fun i (s, _) -> (s, func_params.(i))) func.args
    in
    let ret_val = compile_body ibuilder named_params func.body in
    let _ =
      match func.ret_type with
      | "unit" -> build_ret_void ibuilder
      | _ -> build_ret ret_val ibuilder
    in
    assert_valid_function func_def
    (*
    let _ = build_call print [| str_gep |] "2" ibuilder in
    let _ = build_call print [| str_gep |] "3" ibuilder in
    let _ = build_ret_void ibuilder in
*)
  in

  (* compile the program *)
  List.iter compile_func prog.func_decls;

  (* dump program to llvm ir *)
  assert_valid_module prog_module;
  print_module prog_ll_file prog_module;

  (* link runtime and make executable *)
  ignore
    (Sys.command
       (Printf.sprintf "clang %s ../runtime.c -o %s/%s" prog_ll_file bin_dir
          bin_name))
