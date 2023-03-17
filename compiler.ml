open Llvm
open Llvm_analysis
open Ast

(* compile ast to llvm *)
let compile prog bin_name bin_dir =
  (* llvm global variables *)
  let context = create_context () in
  let prog_module = create_module context bin_name in
  let prog_ll_file = Printf.sprintf "%s/%s.ll" bin_dir bin_name in
  set_target_triple "arm64-apple-macosx13.0.0" prog_module;

  (* map of functions declared *)
  let func_table = Hashtbl.create 50 in
  let var_counter = ref 0 in
  let global_counter = ref 0 in

  (* llvm types *)
  let void_type = void_type context in
  let int_type = i32_type context in
  let str_type = i8_type context |> pointer_type in

  (* global declarations *)
  let print =
    declare_function "printf"
      (function_type (i32_type context) [| pointer_type (i8_type context) |])
      prog_module
  in

  let memalign =
    declare_function "posix_memalign"
      (function_type int_type [| pointer_type str_type; int_type; int_type |])
      prog_module
  in

  let fiber1 =
    declare_function "fiber1"
      (function_type int_type [| int_type; str_type; str_type; str_type |])
      prog_module
  in

  let dummy_personality =
    declare_function "dummy_personality"
      (function_type void_type [||])
      prog_module
  in

  let exn = declare_global str_type "exn" prog_module in

  let continue =
      declare_function "continue" (function_type int_type [|
      str_type|]) prog_module
  in

  let perform =
    declare_function "perform"
      (function_type int_type [| int_type |])
      prog_module
  in

  let exn_type = struct_type context [| int_type; str_type; str_type |] in

  Hashtbl.add func_table "print" print;

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

  let rec compile_body entry_bb params exit_bb lpad_bb func body =
    let ibuilder = builder_at_end context entry_bb in
    let rec compile_body_aux = function
      | Const (n, TyInt) ->
          let ret = const_int int_type (int_of_string n) in
          (ret, [ entry_bb ])
      | Const (s, TyString) ->
          let global_name = get_new_global () in
          let global_value =
            define_global global_name
              (const_stringz context (s ^ "\n"))
              prog_module
          in
          let ret =
            build_gep global_value
              [| const_int int_type 0; const_int int_type 0 |]
              (get_new_var ()) ibuilder
          in
          (ret, [ entry_bb ])
      | Var v ->
          let ret =
            Array.find_opt (fun (s, _) -> s = v) params |> Option.get |> snd
          in
          (ret, [ entry_bb ])
      | BinOp (e1, e2, name) -> (
          match name with
          | "+" ->
              let ret =
                build_add
                  (compile_body_aux e1 |> fst)
                  (compile_body_aux e2 |> fst)
                  (get_new_var ()) ibuilder
              in
              (ret, [ entry_bb ])
          | _ -> failwith "invalid op")
      | FunApp { func_name; args } -> (
          match func_name with
          | "perform" ->
              let ret =
                build_call perform
                  [| const_int int_type 0 |]
                  (get_new_var ()) ibuilder
              in
              (ret, [ entry_bb ])
          | _ ->
              let ret =
                build_call
                  (Hashtbl.find func_table func_name)
                  (Array.map (fun s -> compile_body_aux s |> fst) args)
                  (get_new_var ()) ibuilder
              in
              (ret, [ entry_bb ]))
      | Let { bind_var; bind_expr; body } -> (
          match bind_var with
          | Any ->
              let _ = compile_body_aux bind_expr |> fst in
              let ret = compile_body_aux body |> fst in
              (ret, [ entry_bb ])
          | Var v ->
              let bind_val, next_bb = compile_body_aux bind_expr in
              if List.length next_bb > 0 then
                let ret, next_bb =
                  compile_body (List.hd next_bb)
                    (Array.concat [ [| (v, bind_val) |]; params ])
                    exit_bb lpad_bb func body
                in
                (ret, next_bb)
              else
                let ret =
                  compile_body entry_bb
                    (Array.concat [ [| (v, bind_val) |]; params ])
                    exit_bb lpad_bb func body
                  |> fst
                in
                (ret, [ entry_bb ])
          | _ -> failwith "binding a non var or wildcard")
      | Handle { body = FunApp { func_name; args }; _ } ->
          (* get lpad address for top level func *)
          let lpad_address =
            build_bitcast
              (block_address func lpad_bb)
              str_type (get_new_var ()) ibuilder
          in
          (* build next bb *)
          let next_bb =
            append_block context
              (Printf.sprintf "block_%s" (get_new_var ()))
              func
          in
          move_block_after entry_bb next_bb;
          (* create new stack and launch fiber *)
          let new_sp = build_alloca str_type (get_new_var ()) ibuilder in
          let _ =
            build_call memalign
              [| new_sp; const_int int_type 16; const_int int_type 16384 |]
              (get_new_var ()) ibuilder
          in
          let new_sp_val = build_load new_sp (get_new_var ()) ibuilder in
          let func = Hashtbl.find func_table func_name in
          let func_address =
            build_bitcast func str_type (get_new_var ()) ibuilder
          in
          let ret =
            build_invoke fiber1
              (Array.concat
                 [
                   Array.map (fun s -> compile_body_aux s |> fst) args;
                   [| new_sp_val; func_address; lpad_address |];
                 ])
              next_bb lpad_bb (get_new_var ()) ibuilder
          in
          (ret, [ next_bb ])
      | _ -> failwith "Invalid parse value"
    in
    compile_body_aux body
  in
  let compile_func func =
    (* util functions *)
    let func_type =
      function_type (ret_type func.ret_type)
        (Array.map (fun (_, s) -> ret_type s) func.args)
    in
    let func_def = define_function func.name func_type prog_module in
    let entry_bb = entry_block func_def in

    (* add function to declarations *)
    Hashtbl.add func_table func.name func_def;

    (* compile the body *)
    let func_params = params func_def in
    Array.iteri (fun i (s, _) -> set_value_name s func_params.(i)) func.args;
    let named_params =
      Array.mapi (fun i (s, _) -> (s, func_params.(i))) func.args
    in
    let exit_bb = append_block context "exit" func_def in
    let exit_builder = builder_at_end context exit_bb in

    (* create landing pad *)
    let lpad_bb = append_block context "lpad" func_def in
    let lpad_builder = builder_at_end context lpad_bb in
    let landingpad =
      build_landingpad exn_type dummy_personality 1 (get_new_var ())
        lpad_builder
    in
    let _ = add_clause landingpad (const_bitcast exn (pointer_type int_type)) in
    let eff_val =
      build_extractvalue landingpad 0 (get_new_var ()) lpad_builder
    in
    let eff_obj =
      build_extractvalue landingpad 1 (get_new_var ()) lpad_builder
    in
    let _ = build_call continue [| eff_obj |] (get_new_var ()) lpad_builder in
    let _ = build_unreachable lpad_builder in
    let ret_val, unwind_blocks =
      compile_body entry_bb named_params exit_bb lpad_bb func_def func.body
    in
    let entry_builder =
      builder_at_end context (try List.hd unwind_blocks with _ -> entry_bb)
    in
    (match func.body with _ -> build_br exit_bb entry_builder |> ignore);
    (match func.ret_type with
    | "unit" -> build_ret_void exit_builder |> ignore
    | _ -> build_ret ret_val exit_builder |> ignore);
    assert_valid_function func_def
  in

  (* compile the program *)
  List.iter compile_func prog.func_decls;

  (* dump program to llvm ir *)
  assert_valid_module prog_module;
  print_module prog_ll_file prog_module;

  (* link runtime and make executable *)
  ignore
    (Sys.command
       (Printf.sprintf "clang %s ../runtime.s -o %s/%s" prog_ll_file bin_dir
          bin_name))
