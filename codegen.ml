(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm
open List

exception Error of string

let context = global_context ()
let the_module = create_module context "my cool jit"
let builder = builder context
let fcount = ref 0

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
  | Ast.Array (n, t) -> array_type (type_for t) n
  | Ast.Function (args, t) -> function_type (type_for t) (Array.of_list (map type_for args))
  | Ast.Undefined -> void_type context

let assign_params f args env =
  let iter i value =
    let name = nth args i in
    set_value_name name value;
    Hashtbl.add env name value in
  Array.iteri iter (params f)

let generate_function env types ret_type = 
  let types' = map type_for types in
  let fun_type = function_type (type_for ret_type) (Array.of_list types') in
  let name = String.concat "-" ["fun"; Int32.to_string (Int32.of_int !fcount)] in
  fcount := !fcount + 1;
  declare_function name fun_type the_module

let rec generate env e =
  match e with
  | Ast.FloatLiteral n -> const_float (double_type context) n
  | Ast.IntLiteral n -> const_int (i32_type context) n
  | Ast.ArrayLiteral (list, Ast.Array (n, t)) ->
     let vals = map (generate env) list in
     const_array (type_for t) (Array.of_list vals)
     
  | Ast.Sequence (list, _) ->
     let list' = map (generate env) list in
     nth list' ((length list') - 1)

  | Ast.Call (name, [], _) -> lookup env name
     
  | Ast.Call ("+", [lhs; rhs], Ast.Int) ->
     build_add (generate env lhs) (generate env rhs) "addtmp" builder

  | Ast.Call ("+", [lhs; rhs], Ast.Float) ->
     build_fadd (generate env lhs) (generate env rhs) "addtmp" builder

  | Ast.Call (name, args, ret_type) ->
     let callee = lookup env name in 
     let args' = map (generate env) args in
     build_call callee (Array.of_list args') "calltmp" builder

  | Ast.Def (name, expr, t) ->
     let value = generate env expr in
     Hashtbl.add env name value;
     value

  | Ast.Fun (args, types, body, ret_type) ->
     let env = new_env in
     let f = generate_function env types ret_type in
     let bb = append_block context "entry" f in

     assign_params f args env;
     position_at_end bb builder;

     try
       let ret_val = generate env body in
       let _ = build_ret ret_val builder in
       Llvm_analysis.assert_valid_function f;
       f
     with e ->
       delete_function f;
       raise e
