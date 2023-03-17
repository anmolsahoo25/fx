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
  let eff_table = Hashtbl.create 50 in

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
      (function_type int_type [| str_type; str_type; str_type; str_type |])
      prog_module
  in

  let dummy_personality =
    declare_function "dummy_personality"
      (function_type void_type [||])
      prog_module
  in

  let exn = declare_global str_type "exn" prog_module in

  let continue =
    declare_function "continue"
      (function_type int_type [| str_type; str_type |])
      prog_module
  in

  let perform =
    declare_function "perform"
      (function_type str_type [| int_type; str_type |])
      prog_module
  in

  let exn_type = struct_type context [| str_type; str_type |] in

  let err_msg =
    define_global "err_msg"
      (const_string context "unhandled\n  effect\n")
      prog_module
  in

  Hashtbl.add func_table "print" print;
  Hashtbl.add eff_table "get" 0;
  Hashtbl.add eff_table "put" 1;

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

  let rec compile_body entry_bb params exit_bb func body =
    let ibuilder = builder_at_end context entry_bb in
    let rec compile_body_aux = function
      | Null -> (const_null str_type, [ entry_bb ])
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
      | MutVar v ->
          let ptr =
            Array.find_opt (fun (s, _) -> s = v) params |> Option.get |> snd
          in
          let ret = build_load ptr (get_new_var ()) ibuilder in
          (ret, [ entry_bb ])
      | BinOp (e1, e2, name) -> (
          match name with
          | "+" ->
              let b1 = compile_body_aux e1 |> fst in
              let b2 = compile_body_aux e2 |> fst in
              let b1_cast =
                build_ptrtoint b1 int_type (get_new_var ()) ibuilder
              in
              let b2_cast =
                build_ptrtoint b2 int_type (get_new_var ()) ibuilder
              in
              let ret = build_add b1_cast b2_cast (get_new_var ()) ibuilder in
              (ret, [ entry_bb ])
          | _ -> failwith "invalid op")
      | FunApp { func_name; args } -> (
          match func_name with
          | "perform" ->
              (* check effect performed *)
              let eff_typ =
                match Array.get args 0 with
                | Const (s, TyString) ->
                    const_int int_type (Hashtbl.find eff_table s)
                | _ -> failwith "undeclared effect used"
              in

              (* allocate eff arg *)
              let eff_arg = compile_body_aux (Array.get args 1) |> fst in
              let eff_arg =
                build_inttoptr eff_arg str_type (get_new_var ()) ibuilder
              in

              (* allocate eff obj *)
              let exn_obj = build_malloc exn_type (get_new_var ()) ibuilder in
              let exn_obj_val =
                const_struct context [| eff_arg; const_null str_type |]
              in

              (* store eff val in obj *)
              let _ = build_store exn_obj_val exn_obj ibuilder in
              let exn_pointer =
                build_bitcast exn_obj str_type (get_new_var ()) ibuilder
              in

              (* call perform *)
              let ret =
                build_call perform [| eff_typ; exn_pointer |] (get_new_var ())
                  ibuilder
              in
              (ret, [ entry_bb ])
          | "continue" ->
              (* resume effect *)
              let ret =
                build_call continue
                  (Array.map
                     (fun s ->
                       let ret = compile_body_aux s |> fst in
                       let build_cast =
                         build_inttoptr ret str_type (get_new_var ()) ibuilder
                       in
                       build_cast)
                     args)
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
              let _, next_bb = compile_body_aux bind_expr in
              if List.length next_bb > 0 then
                let ret, next_bb =
                  compile_body (List.hd next_bb) params exit_bb func body
                in
                (ret, next_bb)
              else
                let ret =
                  compile_body entry_bb params exit_bb func body |> fst
                in
                (ret, [ entry_bb ])
          | Var v ->
              let bind_val, next_bb = compile_body_aux bind_expr in
              if List.length next_bb > 0 then
                let ret, next_bb =
                  compile_body (List.hd next_bb)
                    (Array.concat [ [| (v, bind_val) |]; params ])
                    exit_bb func body
                in
                (ret, next_bb)
              else
                let ret =
                  compile_body entry_bb
                    (Array.concat [ [| (v, bind_val) |]; params ])
                    exit_bb func body
                  |> fst
                in
                (ret, [ entry_bb ])
          | MutVar v ->
              let mut_var_val =
                build_alloca str_type (get_new_var ()) ibuilder
              in
              let bind_val, next_bb = compile_body_aux bind_expr in
              let val_cast =
                build_inttoptr bind_val str_type (get_new_var ()) ibuilder
              in
              let _ = build_store val_cast mut_var_val ibuilder in
              if List.length next_bb > 0 then
                let ret, next_bb =
                  compile_body (List.hd next_bb)
                    (Array.concat [ [| (v, mut_var_val) |]; params ])
                    exit_bb func body
                in
                (ret, next_bb)
              else
                let ret =
                  compile_body entry_bb
                    (Array.concat [ [| (v, mut_var_val) |]; params ])
                    exit_bb func body
                  |> fst
                in
                (ret, [ entry_bb ])
          | _ -> failwith "binding a non var or wildcard")
      | Handle { body = FunApp { func_name; args }; branches } ->
          (* create landing pad *)
          let lpad_bb_label = Printf.sprintf "lpad_%s" (get_new_var ()) in
          let lpad_bb = append_block context lpad_bb_label func in
          let lpad_builder = builder_at_end context lpad_bb in
          let landingpad =
            build_landingpad exn_type dummy_personality 1 (get_new_var ())
              lpad_builder
          in

          (* add catch all clause *)
          let _ =
            add_clause landingpad (const_bitcast exn (pointer_type int_type))
          in

          (* extract variables passed by perform *)
          let eff_typ =
            build_extractvalue landingpad 0 (get_new_var ()) lpad_builder
          in
          let eff_typ =
            build_pointercast eff_typ int_type (get_new_var ()) lpad_builder
          in
          let eff_obj =
            build_extractvalue landingpad 1 (get_new_var ()) lpad_builder
          in
          let cast_eff_val_ptr =
            build_bitcast eff_obj
              (pointer_type @@ i64_type context)
              (get_new_var ()) lpad_builder
          in
          let load_eff_val =
            build_load cast_eff_val_ptr (get_new_var ()) lpad_builder
          in

          let load_eff_val =
            build_inttoptr load_eff_val str_type (get_new_var ()) lpad_builder
          in

          (* compile handler branches *)
          let branch_bbs =
            Array.mapi
              (fun i (bindings, body) ->
                (* append new block for lpad continue *)
                let branch_bb =
                  append_block context
                    (Printf.sprintf "%s_%d" lpad_bb_label i)
                    func
                in
                let branch_builder = builder_at_end context branch_bb in

                (* compile body *)
                let _, next_bb =
                  compile_body branch_bb
                    (Array.concat
                       [ [| ("k", eff_obj); ("v", load_eff_val) |]; params ])
                    exit_bb func body
                in

                if List.length next_bb > 0 then
                  let next_builder = builder_at_end context (List.hd next_bb) in
                  build_unreachable next_builder |> ignore
                else
                  (* terminate lpad *)
                  build_unreachable branch_builder |> ignore;

                (* eff typ *)
                let eff_typ_val =
                  match List.hd bindings with
                  | Const (s, TyString) -> s
                  | _ -> failwith "not an effect"
                in
                (branch_bb, eff_typ_val))
              branches
          in

          (* build unhandled block *)
          let unhandled_bb =
            append_block context
              (Printf.sprintf "%s_unhandled" lpad_bb_label)
              func
          in
          let unhandled_builder = builder_at_end context unhandled_bb in
          let err_arg =
            build_gep err_msg
              [| const_int int_type 0; const_int int_type 0 |]
              (get_new_var ()) unhandled_builder
          in
          let _ =
            build_call print [| err_arg |] (get_new_var ()) unhandled_builder
          in
          let _ = build_unreachable unhandled_builder in

          let switch =
            build_switch eff_typ unhandled_bb (Array.length branches)
              lpad_builder
          in
          Array.iter
            (fun (branch_bb, eff) ->
              let sw_val = const_int int_type (Hashtbl.find eff_table eff) in
              add_case switch sw_val branch_bb |> ignore)
            branch_bbs;

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
                   Array.map
                     (fun s ->
                       let b1 = compile_body_aux s |> fst in
                       let b1_cast =
                         build_inttoptr b1 str_type (get_new_var ()) ibuilder
                       in
                       b1_cast)
                     args;
                   [| new_sp_val; func_address; lpad_address |];
                 ])
              next_bb lpad_bb (get_new_var ()) ibuilder
          in
          (ret, [ next_bb ])
      | Set { var = MutVar v; body } ->
          let ptr =
            Array.find_opt (fun (s, _) -> s = v) params |> Option.get |> snd
          in
          let ret_val, _ = compile_body_aux body in
          let cast_val =
            build_inttoptr ret_val str_type (get_new_var ()) ibuilder
          in
          let ret = build_store cast_val ptr ibuilder in
          (ret, [ entry_bb ])
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

    (* compile the function body *)
    let ret_val, unwind_blocks =
      compile_body entry_bb named_params exit_bb func_def func.body
    in
    let entry_builder =
      builder_at_end context (try List.hd unwind_blocks with _ -> entry_bb)
    in
    (match func.body with _ -> build_br exit_bb entry_builder |> ignore);
    (match func.ret_type with
    | "unit" -> build_ret_void exit_builder |> ignore
    | _ ->
        let cast_val =
          build_ptrtoint ret_val int_type (get_new_var ()) exit_builder
        in
        build_ret cast_val exit_builder |> ignore);
    assert_valid_function func_def
  in

  (* compile the program *)
  List.iter compile_func prog.func_decls;

  (* dump program to llvm ir *)
  print_module prog_ll_file prog_module;

  (* link runtime and make executable *)
  ignore
    (Sys.command
       (Printf.sprintf "clang %s ../runtime.s -o %s/%s" prog_ll_file bin_dir
          bin_name))
