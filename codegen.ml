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

let struct_type_for llvm_types =
  struct_type context (Array.of_list llvm_types)

let struct_ptr value members name =
  let index = assoc name (mapi (fun i (name, t) -> (name, i)) members) in
  build_struct_gep value index "mem" builder

let string_type =
  struct_type context (Array.of_list [i32_type context; pointer_type (i8_type context)])
                              
let rec llvm_type_for = function
  | Float -> float_type context
  | Double -> double_type context
  | Bool -> i1_type context
  | Byte -> i8_type context
  | Int16 -> i16_type context
  | Int32 -> i32_type context
  | Int -> i64_type context
  | Void -> void_type context
  | TypeRef _ -> raise (Error "unresolved type ref")
  | Struct members -> pointer_type (struct_type_for (map llvm_type_for (map snd members)))
  | String -> pointer_type string_type
  | Pointer t -> pointer_type (llvm_type_for t)
  | Function (args, t) -> function_type (llvm_type_for t) (Array.of_list (map llvm_type_for args))
  | Undefined -> void_type context

let init_env env =
  Hashtbl.add env ("+", [Int; Int]) ("+", Int);
  Hashtbl.add env ("to_i", [Float]) ("ftoi", Int);
  Hashtbl.add env ("to_f", [Int]) ("itof", Float);
  Hashtbl.add env ("+", [Float; Float]) ("+", Float);
  Hashtbl.add env ("-", [Int; Int]) ("-", Int);
  Hashtbl.add env ("-", [Float; Float]) ("-", Float);
  Hashtbl.add env ("+", [String; String]) ("+", String)

let declare_extern m code_env type_env =
  let declare ret name name' args =
    let ftype = function_type (llvm_type_for ret)
                              (Array.of_list (map llvm_type_for args)) in
    let f = declare_function name' ftype m in
    Hashtbl.add type_env (name, args) (name', ret);
    Hashtbl.add code_env (name', args) f in
  declare Void "puts" "string_puts" [String];
  declare String "string_new" "string_new" [Pointer Byte; Int32];
  declare Int "len" "string_len" [String];
  declare String "+" "string_add" [String; String];
  declare Int "to_i" "string_to_int" [String];
  declare Float "to_f" "string_to_float" [String]

let assign_params f args env =
  let iter i value =
    let name = nth args i in
    set_value_name name value;
    Hashtbl.add env (name, []) value in
  Array.iteri iter (params f)

let make_call env name args arg_types =
  let callee = lookup env name arg_types in
  build_call callee (Array.of_list args) "" builder

let make_ret ret_type ret =
  match ret_type with
  | Void -> build_ret_void builder
  | _ -> build_ret ret builder

let rec generate m env = function
  | StructDef (_, _) -> const_int (i32_type context) 0
  | FloatLiteral n -> const_float (float_type context) n
  | IntLiteral n -> const_int (i64_type context) n
  | StringLiteral s ->
     let init = build_global_stringptr s "str" builder in
     let args = [init; const_int (i32_type context) (String.length s)] in
     make_call env "string_new" args [Pointer Byte; Int32]

  | StructLiteral (members, Struct member_types) ->
     let struct_type = struct_type_for (map llvm_type_for (map snd member_types)) in
     let struct_val = build_malloc struct_type "struct" builder in
     let build_member i (_, e) =
       let v = generate m env e in
       let p = build_struct_gep struct_val i "" builder in
       let _ = build_store v p builder in
       () in
     iteri build_member members;
     struct_val

  | StructLiteral (_, _) -> raise (Error "badly typed struct literal")
     
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

  | Call ("-", [lhs; rhs], Int) ->
     build_sub (generate m env lhs) (generate m env rhs) "sub" builder

  | Call ("-", [lhs; rhs], Float) ->
     build_fsub (generate m env lhs) (generate m env rhs) "sub" builder

  | Call ("ftoi", [expr], _) ->
     build_fptosi (generate m env expr) (i64_type context) "" builder
                
  | Call ("itof", [expr], _) ->
     build_sitofp (generate m env expr) (float_type context) "" builder

  | Mem (expr, name, _) ->
     let expr' = generate m env expr in
     begin
       match Types.type_of expr with
       | Struct members ->
          let ptr = struct_ptr expr' members name in
          build_load ptr "" builder
       | _ -> raise (Error "no struct type")
     end

  | MemSet (lhs, name, rhs, _) ->
     let lhs' = generate m env lhs in
     let rhs' = generate m env rhs in
     begin
       match Types.type_of lhs with
       | Struct members ->
          let ptr = struct_ptr lhs' members name in
          build_store rhs' ptr builder
       | _ -> raise (Error "no struct type")
     end
                                  
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
       let _ = make_ret (Types.type_of (last body)) (last body') in
       Llvm_analysis.assert_valid_function func;
       func
     with e ->
       delete_function func;
       raise e
