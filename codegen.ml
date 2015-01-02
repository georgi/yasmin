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
  Hashtbl.add env ("to_i", [Float]) ("__ftoi__", Int);
  Hashtbl.add env ("to_f", [Int]) ("__itof__", Float);
  Hashtbl.add env ("==", [Float; Float]) (":fcmp_eq", Bool);
  Hashtbl.add env ("!=", [Float; Float]) (":fcmp_ne", Bool);
  Hashtbl.add env ("<", [Float; Float]) (":fcmp_lt", Bool);
  Hashtbl.add env (">", [Float; Float]) (":fcmp_gt", Bool);
  Hashtbl.add env ("<=", [Float; Float]) (":fcmp_le", Bool);
  Hashtbl.add env (">=", [Float; Float]) (":fcmp_ge", Bool);
  Hashtbl.add env ("==", [Int; Int]) (":icmp_eq", Bool);
  Hashtbl.add env ("!=", [Int; Int]) (":icmp_ne", Bool);
  Hashtbl.add env ("<", [Int; Int]) (":icmp_lt", Bool);
  Hashtbl.add env (">", [Int; Int]) (":icmp_gt", Bool);
  Hashtbl.add env ("<=", [Int; Int]) (":icmp_le", Bool);
  Hashtbl.add env (">=", [Int; Int]) (":icmp_ge", Bool);
  Hashtbl.add env ("+", [Int; Int]) (":add", Int);
  Hashtbl.add env ("-", [Int; Int]) (":sub", Int);
  Hashtbl.add env ("*", [Int; Int]) (":mul", Int);
  Hashtbl.add env ("+", [Float; Float]) (":fadd", Float);
  Hashtbl.add env ("-", [Float; Float]) (":fsub", Float);
  Hashtbl.add env ("*", [Float; Float]) (":fmul", Float);
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

let binop_builder = function
  | ":add" -> build_add
  | ":sub" -> build_sub
  | ":mul" -> build_mul
  | ":div" -> build_sdiv
  | ":fadd" -> build_fadd
  | ":fsub" -> build_fsub
  | ":fmul" -> build_fmul
  | ":fdiv" -> build_fdiv
  | ":fcmp_eq" -> build_fcmp Fcmp.Oeq
  | ":fcmp_ne" -> build_fcmp Fcmp.One
  | ":fcmp_lt" -> build_fcmp Fcmp.Olt
  | ":fcmp_gt" -> build_fcmp Fcmp.Ogt
  | ":fcmp_le" -> build_fcmp Fcmp.Ole
  | ":fcmp_ge" -> build_fcmp Fcmp.Oge
  | ":icmp_ne" -> build_icmp Icmp.Ne
  | ":icmp_eq" -> build_icmp Icmp.Eq
  | ":icmp_lt" -> build_icmp Icmp.Slt
  | ":icmp_gt" -> build_icmp Icmp.Sgt
  | ":icmp_le" -> build_icmp Icmp.Sle
  | ":icmp_ge" -> build_icmp Icmp.Sge
  | _ -> raise (Error "unknown binary operator")

let rec generate func _module env expr =
  let gen = generate func _module env in
  match expr with
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
       let p = build_struct_gep struct_val i "" builder in
       let _ = build_store (gen e) p builder in
       () in
     iteri build_member members;
     struct_val

  | StructLiteral (_, _) -> raise (Error "badly typed struct literal")
     
  (* | ArrayLiteral (list, Array (n, t)) -> *)
  (*    let vals = map (generate m env) list in *)
  (*    const_array (llvm_type_for t) (Array.of_list vals) *)

  | New (size, t) ->
     let size' = gen size in
     let ltype = llvm_type_for t in
     build_array_malloc ltype size' "malloc" builder

  | Let (name, expr, t) ->
     let value = gen expr in
     Hashtbl.add env (name, []) value;
     set_value_name name value;
     value
     
  | Var (name, _) ->
     lookup env name []

  | Mem (expr, name, _) ->
     let expr' = gen expr in
     begin
       match Types.type_of expr with
       | Struct members ->
          let ptr = struct_ptr expr' members name in
          build_load ptr "" builder
       | _ -> raise (Error "no struct type")
     end

  | MemSet (lhs, name, rhs, _) ->
     let lhs' = gen lhs in
     let rhs' = gen rhs in
     begin
       match Types.type_of lhs with
       | Struct members ->
          let ptr = struct_ptr lhs' members name in
          build_store rhs' ptr builder
       | _ -> raise (Error "no struct type")
     end
                
  | Call ("__itof__", [expr], _) ->
     build_sitofp (gen expr) (float_type context) "" builder

  | Call ("__ftoi__", [expr], _) ->
     build_fptosi (gen expr) (i64_type context) "" builder

  | Call ("[]", [array; index], _) ->
     let array' = gen array in
     let index' = Array.of_list [(gen index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_load ptr "ptr" builder

  | Call ("[]=", [array; index; expr], _) ->
     let index' = Array.of_list [gen index] in
     let ptr = build_gep (gen array) index' "ary" builder in
     build_store (gen expr) ptr builder

  | Call (name, args, _) ->
     if String.contains name ':' then
       let f = binop_builder name in
       match args with
       | [lhs; rhs] -> f (gen lhs) (gen rhs) "" builder
       | _ -> raise (Error "binop with wrong arg count")
     else
       let args' = map gen args in
       let arg_types = map Types.type_of args in
       make_call env name args' arg_types

  | If (cond, then_clause, else_clause, t) ->
     let result = build_alloca (llvm_type_for t) "res" builder in
     let then_block = append_block context "then" func in
     let else_block = append_block context "else" func in
     let exit_block = append_block context "exit" func in
     let _ = build_cond_br (gen cond) then_block else_block builder in
     position_at_end then_block builder;
     let then_result = last (map gen then_clause) in
     let _ = build_store then_result result builder in
     let _ = build_br exit_block builder in
     position_at_end else_block builder;
     let else_result = last (map gen else_clause) in
     let _ = build_store else_result result builder in
     let _ = build_br exit_block builder in
     position_at_end exit_block builder;
     build_load result "res" builder

let generate_function m env name args types body ret_type =
  let env' = Hashtbl.copy env in
  let types' = map llvm_type_for types in
  let fun_type = function_type (llvm_type_for ret_type) (Array.of_list types') in
  let func = declare_function name fun_type m in
  let block = append_block context "entry" func in

  Hashtbl.add env (name, types) func;
  assign_params func args env';
  position_at_end block builder;

  try
    let body' = map (generate func m env') body in
    let _ = make_ret (Types.type_of (last body)) (last body') in
    Llvm_analysis.assert_valid_function func;
    func
  with e ->
    delete_function func;
    raise e
