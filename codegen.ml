open Llvm
open List
open Ast

exception Error of string

let context = global_context ()
let builder = builder context
let last list = nth list ((length list) - 1)

let lookup env name args =
  (try Hashtbl.find env (name, args) with
   | Not_found -> raise (Error ("unknown variable " ^ name ^ (Types.string_of_types args))))

let rec llvm_type_for = function
  | Float -> float_type context
  | Bool -> i1_type context
  | Byte -> i8_type context
  | Int16 -> i16_type context
  | Int32 -> i32_type context
  | Int -> i64_type context
  | Void -> void_type context
  | String -> pointer_type (struct_type context (Array.of_list [i32_type context; pointer_type (i8_type context)]))
  | Pointer t -> pointer_type (llvm_type_for t)
  | Function (args, t) -> function_type (llvm_type_for t) (Array.of_list (map llvm_type_for args))
  | Undefined -> void_type context

let declare_extern m code_env type_env =
  let declare ret name name' args =
    let ftype = function_type (llvm_type_for ret)
                              (Array.of_list (map llvm_type_for args)) in
    let f = declare_function name ftype m in
    Hashtbl.add type_env (name', args) (name, ret);
    Hashtbl.add code_env (name, args) f in
  declare Void "string_puts" "puts" [String];
  declare String "string_new" "string_new" [Pointer Byte; Int32];
  declare String "string_add" "+" [String; String];
  declare Void "free" "free" [String]

let assign_params f args env =
  let iter i value =
    let name = nth args i in
    set_value_name name value;
    Hashtbl.add env (name, []) value in
  Array.iteri iter (params f)

let make_call env name args arg_types =
  let callee = lookup env name arg_types in
  build_call callee (Array.of_list args) "" builder

let rec generate m env = function
  | FloatLiteral n -> const_float (double_type context) n
  | IntLiteral n -> const_int (i64_type context) n
  | StringLiteral s ->
     let init = build_global_stringptr s "str" builder in
     let args = [init; const_int (i32_type context) (String.length s)] in
     make_call env "string_new" args [Pointer Byte; Int32]

  (* | ArrayLiteral (list, Array (n, t)) -> *)
  (*    let vals = map (generate m env) list in *)
  (*    const_array (llvm_type_for t) (Array.of_list vals) *)
                                  
  | New (size, t) ->
     let size' = generate m env size in
     let ltype = llvm_type_for t in
     build_array_malloc ltype size' "malloc" builder

  | Let (name, expr, t) ->
     let value = generate m env expr in
     Hashtbl.add env (name, []) value;
     set_value_name name value;
     value
     
  | Var (name, _) ->
     lookup env name []

  | Call ("+", [lhs; rhs], Int) ->
     build_add (generate m env lhs) (generate m env rhs) "add" builder

  | Call ("+", [lhs; rhs], Float) ->
     build_fadd (generate m env lhs) (generate m env rhs) "add" builder
                
  | Call ("[]", [array; index], _) ->
     let array' = generate m env array in
     let index' = Array.of_list [(generate m env index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_load ptr "ptr" builder

  | Call ("[]=", [array; index; expr], _) ->
     let expr' = generate m env expr in
     let array' = generate m env array in
     let index' = Array.of_list [(generate m env index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_store expr' ptr builder

  | Call (name, args, _) ->
     let args' = map (generate m env) args in
     let arg_types = map Types.type_of args in
     make_call env name args' arg_types

  | Fun (name, args, types, body, ret_type) ->
     let env' = Hashtbl.copy env in
     let types' = map llvm_type_for types in
     let fun_type = function_type (llvm_type_for ret_type) (Array.of_list types') in
     let func = declare_function name fun_type m in
     let block = append_block context "entry" func in

     Hashtbl.add env (name, types) func;
     assign_params func args env';
     position_at_end block builder;

     try
       let body' = map (generate m env') body in
       let ret = last body' in
       let _ = if ret_type = Void then
                 build_ret_void builder else build_ret ret builder in
       Llvm_analysis.assert_valid_function func;
       func
     with e ->
       delete_function func;
       raise e
