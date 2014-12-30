(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
open List

exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context

let kind_of t = classify_type (type_of t)

let new_env:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let lookup env name =
  (try Hashtbl.find env name with
   | Not_found -> raise (Error "unknown variable name"))

let lookup_callee name =
  match lookup_function name the_module with
  | Some name -> name
  | None -> raise (Error "unknown function referenced")

let rec type_for = function
  | Ast.Float -> float_type context
  | Ast.Bool -> i1_type context
  | Ast.Byte -> i8_type context
  | Ast.Int -> i32_type context
  | Ast.Void -> void_type context
  | Ast.Array (t, n) -> array_type (type_for t) n
  | Ast.Function (args, t) -> function_type (type_for t) (Array.of_list (map type_for args))
  | Ast.Undefined -> void_type context

let assign_params f args env =
  let iter i value =
    let name = nth args i in
    set_value_name name value;
    Hashtbl.add env name value in
  Array.iteri iter (params f)

let rec generate env = function
  | Ast.FloatLiteral n -> const_float (double_type context) n
  | Ast.IntLiteral n -> const_int (i32_type context) n

  (* | Ast.ArrayLiteral (list, Ast.Array (n, t)) -> *)
  (*    let vals = map (generate env) list in *)
  (*    const_array (type_for t) (Array.of_list vals) *)

  | Ast.Let (t, name, expr, body, t') ->
     let value = generate env expr in
     let env' = Hashtbl.copy env in
     Hashtbl.add env' name value;
     set_value_name name value;
     generate env' body
     
  | Ast.Sequence (list, _) ->
     let list' = map (generate env) list in
     nth list' ((length list') - 1)

  | Ast.Var (name, _) ->
     lookup env name
     
  | Ast.Call ("+", [lhs; rhs], Ast.Int) ->
     build_add (generate env lhs) (generate env rhs) "add" builder

  | Ast.Call ("+", [lhs; rhs], Ast.Float) ->
     build_fadd (generate env lhs) (generate env rhs) "add" builder

  | Ast.Call (name, args, ret_type) ->
     let callee = lookup env name in
     let args' = map (generate env) args in
     build_call callee (Array.of_list args') "call" builder

  | Ast.Fun (name, args, types, body, ret_type) ->
     let env' = Hashtbl.copy env in
     let types' = map type_for types in
     let fun_type = function_type (type_for ret_type) (Array.of_list types') in
     let func = declare_function name fun_type the_module in
     let block = append_block context "entry" func in

     Hashtbl.add env name func;
     assign_params func args env';
     position_at_end block builder;

     try
       let ret = generate env' body in
       let _ = build_ret ret builder in
       Llvm_analysis.assert_valid_function func;
       func
     with e ->
       delete_function func;
       raise e
