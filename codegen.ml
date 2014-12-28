(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Llvm

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

let type_for = function
  | Ast.Float -> float_type context
  | Ast.Bool -> i1_type context
  | Ast.Byte -> i8_type context
  | Ast.Int -> i32_type context
  | Ast.Void -> void_type context
  | Ast.Undefined -> void_type context

let assign_env f arg_defs env =
  let iter i value =
    let (name, _) = List.nth arg_defs i in
    set_value_name name value;
    Hashtbl.add env name value in
  Array.iteri iter (params f)

let generate_proto (name, arg_defs, ret_type) env = 
  let arg_types = List.map (fun (_, t) -> type_for t) arg_defs in
  let ft = function_type (type_for ret_type) (Array.of_list arg_types) in
  let f = match lookup_function name the_module with
    | None -> declare_function name ft the_module
    | Some f -> raise (Error "redefinition of function") in
  assign_env f arg_defs env;
  f

let rec generate e env = match e with
  | Ast.FloatLiteral n -> const_float (double_type context) n
  | Ast.IntLiteral n -> const_int (i32_type context) n
  | Ast.Variable (name, _) -> lookup env name
  | Ast.Sequence list ->
     let vals = List.map (fun e -> generate e env) list in
     List.nth vals ((List.length vals) - 1)
     
  | Ast.Call ("+", [lhs; rhs], Ast.Int) ->
     build_add (generate lhs env) (generate rhs env) "addtmp" builder

  | Ast.Call ("+", [lhs; rhs], Ast.Float) ->
     build_fadd (generate lhs env) (generate rhs env) "addtmp" builder

  | Ast.Call (name, args, ret_type) ->
     let callee = lookup_callee name in 
     let args = List.map (fun a -> generate a env) args in
     build_call callee (Array.of_list args) "calltmp" builder

  | Ast.Function (proto, body) ->
     let env = new_env in
     let the_function = generate_proto proto env in
     let bb = append_block context "entry" the_function in
     
     position_at_end bb builder;

     try
       let ret_val = generate body env in
       let _ = build_ret ret_val builder in

       Llvm_analysis.assert_valid_function the_function;

       the_function
     with e ->
       delete_function the_function;
       raise e
