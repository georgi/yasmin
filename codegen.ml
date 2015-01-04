open Llvm
open List
open Ast

exception Error of string

let context = global_context ()
let builder = builder context
let last list = nth list ((length list) - 1)
  
let rec drop list n =
  match list with
  | [] -> []
  | a :: rest ->
     if n == 0 then list
     else drop rest (n - 1);;

let lookup_function env name args =
  (try Hashtbl.find env (name, args) with
   | Not_found -> raise (Error ("unknown function " ^ name ^ (Types.string_of_types args))))

let lookup_variable env name =
  (try fst(assoc name env) with
   | Not_found -> raise (Error ("unknown variable " ^ name)))

let lookup_variable_type env name =
  (try snd(assoc name env) with
   | Not_found -> raise (Error ("unknown variable " ^ name)))

let create_var_env func args types =
  let bind_param i value =
    let name = nth args i in
    let _type = nth types i in
    set_value_name name value;
    (name, (value, _type)) in
  Array.to_list (Array.mapi bind_param (params func))

let struct_type_for llvm_types =
  struct_type context (Array.of_list llvm_types)

let struct_ptr value members name =
  let index = assoc name (mapi (fun i (name, t) -> (name, i)) members) in
  build_struct_gep value index "mem" builder

let struct_array_type t =
  struct_type_for [i32_type context; i64_type context; pointer_type t]
                              
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
  | Array t -> pointer_type (struct_array_type (llvm_type_for t))
  | Pointer t -> pointer_type (llvm_type_for t)
  | Function (args, t) -> pointer_type (function_type (llvm_type_for t) (Array.of_list (map llvm_type_for args)))
  | Undefined -> void_type context

let rec string_of_env = function 
  | [] -> ""
  | (name, value) :: rest ->
     name ^ ": " ^ (string_of_llvalue value) ^ " " ^ (string_of_env rest)

let init_functions m fun_values fun_types =
  let declare_extern name name' ret args =
    let ftype = function_type (llvm_type_for ret)
                              (Array.of_list (map llvm_type_for args)) in
    let f = declare_function name' ftype m in
    Hashtbl.add fun_types (name, args) (name', ret);
    Hashtbl.add fun_values (name', args) f in

  let declare name name' ret args =
    Hashtbl.add fun_types (name, args) (name', ret) in

  declare "to_i" "__ftoi__" Int [Float];
  declare "to_f" "__itof__" Float [Int];
  declare "==" ":fcmp_eq" Bool [Float; Float];
  declare "!=" ":fcmp_ne" Bool [Float; Float];
  declare "<" ":fcmp_lt" Bool [Float; Float];
  declare ">" ":fcmp_gt" Bool [Float; Float];
  declare "<=" ":fcmp_le" Bool [Float; Float];
  declare ">=" ":fcmp_ge" Bool [Float; Float];
  declare "==" ":icmp_eq" Bool [Int; Int];
  declare "!=" ":icmp_ne" Bool [Int; Int];
  declare "<" ":icmp_lt" Bool [Int; Int];
  declare ">" ":icmp_gt" Bool [Int; Int];
  declare "<=" ":icmp_le" Bool [Int; Int];
  declare ">=" ":icmp_ge" Bool [Int; Int];
  declare "+" ":add" Int [Int; Int];
  declare "-" ":sub" Int [Int; Int];
  declare "*" ":mul" Int [Int; Int];
  declare "+" ":fadd" Float [Float; Float];
  declare "-" ":fsub" Float [Float; Float];
  declare "*" ":fmul" Float [Float; Float];
  declare "and" ":and" Bool [Bool; Bool];
  declare "+" "+" (Array Byte) [(Array Byte); (Array Byte)];

  declare_extern "puts" "string_puts" Void [Array Byte];
  declare_extern "strcpy" "strcpy" (Array Byte) [Array Byte; Array Byte];
  declare_extern "array_new" "array_new" (Array Byte) [Pointer Byte; Int32];
  declare_extern "len" "string_len" Int [Array Byte];
  declare_extern "+" "array_add" (Array Byte) [Array Byte; Array Byte];
  declare_extern "to_i" "string_to_int" Int [Array Byte];
  declare_extern "to_f" "string_to_float" Float [Array Byte]

let make_call fun_values name args arg_types =
  let callee = lookup_function fun_values name arg_types in
  build_call callee (Array.of_list args) "" builder

let make_closure_call var_env name args =
  let closure = lookup_variable var_env name in
  let fun_type = lookup_variable_type var_env name in
  let Function (fun_args, _) = fun_type in
  let ptr = build_struct_gep closure 0 "closure" builder in
  let callee = build_load ptr "" builder in 
  let build_args i _ =
    let p = build_struct_gep closure (i + 1) "" builder in
    build_load p "" builder in
  let closure_args = mapi build_args (drop fun_args (length args)) in
  build_call callee (Array.of_list (args @ closure_args)) "" builder

let make_ret ret_type ret =
  match ret_type with
  | Void -> build_ret_void builder
  | _ -> build_ret ret builder

let binop_builder = function
  | ":and" -> build_and
  | ":or" -> build_or
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

let build_struct_array t buf len =
  let len' = const_int (i32_type context) len in
  let size = size_of (llvm_type_for t) in
  let struct' = build_malloc (struct_array_type (llvm_type_for t)) "arraystruct" builder in
  let _ = build_store len' (build_struct_gep struct' 0 "len" builder) builder in
  let _ = build_store size (build_struct_gep struct' 1 "size" builder) builder in
  let _ = build_store buf (build_struct_gep struct' 2 "buf" builder) builder in
  struct'

let rec generate block _module fun_values var_env expr =
  let gen = generate block _module fun_values var_env in
  match expr with
  | True -> const_int (i1_type context) 1
  | False -> const_int (i1_type context) 0
  | FloatLiteral n -> const_float (float_type context) n
  | IntLiteral n -> const_int (i64_type context) n

  | Seq (seq, t) ->
     let seq' = map gen seq in
     last seq'

  | ArrayLiteral (elements, t) ->
     let len = const_int (i32_type context) (length elements) in
     let buf = build_array_malloc (llvm_type_for t) len "array" builder in
     let struct' = build_struct_array t buf (length elements) in
     let elements' = map gen elements in
     let build_element i v =
       let indices = Array.of_list [const_int (i32_type context) i] in
       let p = build_gep buf indices "array_literal" builder in
       let _ = build_store v p builder in
       () in
     iteri build_element elements';
     struct'

  | StringLiteral s ->
     let init = build_global_stringptr s "string_literal" builder in
     build_struct_array Byte init (String.length s)

  | StructLiteral (members, Struct member_types) ->
     let struct_type = struct_type_for (map llvm_type_for (map snd member_types)) in
     let struct_val = build_malloc struct_type "struct" builder in
     let build_member i (_, e) =
       let p = build_struct_gep struct_val i "structmem" builder in
       let _ = build_store (gen e) p builder in
       () in
     iteri build_member members;
     struct_val

  | StructLiteral (_, _) -> raise (Error "badly typed struct literal")
     
  | New (size, t) ->
     let size' = gen size in
     let ltype = llvm_type_for t in
     build_array_malloc ltype size' "malloc" builder

  | Let (name, expr, body, t) ->
     let value = gen expr in
     let var_env' = (name, (value, Types.type_of expr)) :: var_env in
     set_value_name name value;
     generate block _module fun_values var_env' body
     
  | Var (name, _) ->
     lookup_variable var_env name

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
     let index' = Array.of_list [(const_int (i32_type context) 1); (gen index)] in
     let ptr = build_gep array' index' "ary" builder in
     build_load ptr "ptr" builder

  | Call ("[]=", [array; index; expr], _) ->
     let index' = Array.of_list [(const_int (i32_type context) 1); gen index] in
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
       if mem_assoc name var_env then
         make_closure_call var_env name args'
       else
         make_call fun_values name args' arg_types

  | If (cond, then_clause, else_clause, t) ->
     let func = block_parent block in
     let result = build_alloca (llvm_type_for t) "res" builder in
     let then_block = append_block context "then" func in
     let else_block = append_block context "else" func in
     let exit_block = append_block context "exit" func in
     let _ = build_cond_br (gen cond) then_block else_block builder in
     position_at_end then_block builder;
     let _ = build_store (gen then_clause) result builder in
     let _ = build_br exit_block builder in
     position_at_end else_block builder;
     let _ = build_store (gen else_clause) result builder in
     let _ = build_br exit_block builder in
     position_at_end exit_block builder;
     build_load result "res" builder

  | Fun (name, _, _, types, _, t) ->
     let closure_type = struct_type_for (llvm_type_for t :: (map (fun (_, (_, t)) -> llvm_type_for t) var_env)) in
     let value = build_malloc closure_type "closure" builder in
     let funval = lookup_function fun_values name types in
     let funptr = build_struct_gep value 0 "" builder in
     let _ = build_store funval funptr builder in
     let build_param i (_, (v, _)) =
       let p = build_struct_gep value (i + 1) "" builder in
       let _ = build_store v p builder in
       () in
     iteri build_param var_env;
     value

let generate_function _module fun_values name args types body ret_type =
  let types' = map llvm_type_for types in
  let fun_type = function_type (llvm_type_for ret_type) (Array.of_list types') in
  let func = declare_function name fun_type _module in
  let block = append_block context "entry" func in
  let var_env = create_var_env func args types in

  position_at_end block builder;

  try
    let body' = generate block _module fun_values var_env body in
    let _ = make_ret (Types.type_of body) body' in
    Llvm_analysis.assert_valid_function func;
    func
  with e ->
    delete_function func;
    raise e

let fname = ref 0
                
let rec generate_lambdas _module fun_values var_env expr =
  let gen = generate_lambdas _module fun_values var_env in
  match expr with
  | ArrayLiteral (elements, t) -> ArrayLiteral (map gen elements, t)
  | StructLiteral (members, t) -> StructLiteral (map (fun (n, e) -> (n, gen e)) members, t)
  | Mem (expr, name, t) -> Mem (gen expr, name, t)
  | MemSet (lhs, name, rhs, t) -> MemSet (gen lhs, name, gen rhs, t)
  | Seq (e, t) -> Seq (map gen e, t)
  | Call (name, args, t) -> Call (name, map gen args, t)
  | If (cond, then_clause, else_clause, t) -> If (gen cond, gen then_clause, gen else_clause, t)

  | Let (name, expr, body, t) ->
     let expr' = gen expr in
     let var_env' = (name, Types.type_of expr') :: var_env in
     let body' = generate_lambdas _module fun_values var_env' body in
     Let (name, expr', body', t)

  | Fun (_, args, ret_type, types, body, t) ->
     let args' = args @ (map fst var_env) in
     let types' = types @ (map snd var_env) in
     let name = "lambda-" ^ (string_of_int !fname) in
     let func = generate_function _module fun_values name args' types' body ret_type in
     Hashtbl.add fun_values (name, types') func;
     fname := !fname + 1;
     Fun (name, args', ret_type, types', body, Function (types', ret_type))

  | _ -> expr
