(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
open List

exception Error of string

let context = global_context ()
let builder = builder context
let kind_of t = classify_type (type_of t)
let new_env:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let last list = nth list ((length list) - 1)

(* let declare_extern = *)
(*   let malloc_type = function_type (pointer_type (i8_type context)) (Array.of_list [i32_type context]) in *)
(*   declare_function "malloc" malloc_type the_module *)

let lookup env name =
  (try Hashtbl.find env name with
   | Not_found -> raise (Error ("unknown variable " ^ name)))

let rec llvm_type_for = function
  | Ast.Float -> float_type context
  | Ast.Bool -> i1_type context
  | Ast.Byte -> i8_type context
  | Ast.Int -> i64_type context
  | Ast.Void -> void_type context
  | Ast.Pointer t -> pointer_type (llvm_type_for t)
  | Ast.Function (args, t) -> function_type (llvm_type_for t) (Array.of_list (map llvm_type_for args))
  | Ast.Undefined -> void_type context

let assign_params f args env =
  let iter i value =
    let name = nth args i in
    set_value_name name value;
    Hashtbl.add env name value in
  Array.iteri iter (params f)

let rec generate m env = function
  | Ast.FloatLiteral n -> const_float (double_type context) n
  | Ast.IntLiteral n -> const_int (i64_type context) n

  (* | Ast.ArrayLiteral (list, Ast.Array (n, t)) -> *)
  (*    let vals = map (generate m env) list in *)
  (*    const_array (llvm_type_for t) (Array.of_list vals) *)
                                  
  | Ast.New (expr, t) ->
     let expr' = generate m env expr in
     let ltype = llvm_type_for t in
     build_array_malloc ltype expr' "malloc" builder

  | Ast.Let (t, name, expr, body, t') ->
     let value = generate m env expr in
     let env' = Hashtbl.copy env in
     Hashtbl.add env' name value;
     set_value_name name value;
     let body' = map (generate m env') body in
     last body'
     
  | Ast.Var (name, _) ->
     lookup env name
     
  | Ast.Call ("+", [lhs; rhs], Ast.Int) ->
     build_add (generate m env lhs) (generate m env rhs) "add" builder

  | Ast.Call ("+", [lhs; rhs], Ast.Float) ->
     build_fadd (generate m env lhs) (generate m env rhs) "add" builder
                
  | Ast.Call ("[]", [array; index], _) ->
     let array' = generate m env array in
     let index' = Array.of_list [(generate m env index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_load ptr "ptr" builder

  | Ast.Call ("[]=", [array; index; expr], _) ->
     let expr' = generate m env expr in
     let array' = generate m env array in
     let index' = Array.of_list [(generate m env index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_store expr' ptr builder

  | Ast.Call (name, args, ret_type) ->
     let callee = lookup env name in
     let args' = map (generate m env) args in
     build_call callee (Array.of_list args') "call" builder

  | Ast.Fun (name, args, types, body, ret_type) ->
     let env' = Hashtbl.copy env in
     let types' = map llvm_type_for types in
     let fun_type = function_type (llvm_type_for ret_type) (Array.of_list types') in
     let func = declare_function name fun_type m in
     let block = append_block context "entry" func in

     Hashtbl.add env name func;
     assign_params func args env';
     position_at_end block builder;

     try
       let body' = map (generate m env') body in
       let _ = build_ret (last body') builder in
       Llvm_analysis.assert_valid_function func;
       func
     with e ->
       delete_function func;
       raise e
