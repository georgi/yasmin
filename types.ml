open List
open Ast
       
exception Error of string

let last list = nth list ((length list) - 1)

let var_env args types =
  let env:(string, type_name) Hashtbl.t = Hashtbl.create 10 in
  iter2 (fun name t -> Hashtbl.add env name t) args types;
  env
                                                                                 
let rec string_of_type = function
  | Float -> "float"
  | Double -> "double"
  | Bool -> "bool"
  | Byte -> "byte"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int -> "int"
  | Void -> "void"
  | String -> "string"
  | Array t -> string_of_type t ^ "[]"
  | TypeRef name -> name
  | Struct members -> "struct (" ^ String.concat "," (map (fun (name, t) -> (string_of_type t) ^ " " ^ name) members) ^ ")"
  | Pointer t -> string_of_type t ^ "*"
  | Function (args, t) -> string_of_type t ^ "(" ^ String.concat "," (map string_of_type args) ^ ")"
  | Undefined -> "undefined"

let rec resolve type_map = function
  | TypeRef name -> 
     (try Hashtbl.find type_map name with
      | Not_found -> raise (Error ("could not find type " ^ name)))
  | Array t -> Array (resolve type_map t)
  | Struct members -> Struct (map (fun (n, t) -> (n, resolve type_map t)) members)
  | Pointer t -> Pointer (resolve type_map t)
  | Function (args, t) -> Function (map (resolve type_map) args, resolve type_map t)
  | _ as t -> t

let string_of_types list = "(" ^ String.concat "," (map string_of_type list) ^ ")"

let lookup_variable env name =
  (try Hashtbl.find env name with
   | Not_found -> raise (Error ("could not find variable " ^ name)))

let lookup_function env name types =
  (try Hashtbl.find env (name, types) with
   | Not_found -> raise (Error ("could not find function " ^ name ^ (string_of_types types))))

let type_of = function
  | True -> Bool
  | False -> Bool
  | FloatLiteral _ -> Float
  | IntLiteral _ -> Int
  | StringLiteral _ -> String
  | ArrayLiteral (_, t) -> Array t
  | StructLiteral (_, t) -> t
  | Call (_, _, t) -> t
  | Let (_, _, t) -> t
  | If (_, _, _, t) -> t
  | Var (_, t) -> t
  | Mem (_, _, t) -> t
  | MemSet (_, _, _, t) -> t
  | New (_, t) -> Pointer t
  | Fun (_, _, _, t) -> t

let rec typecheck type_map fun_types var_env e =
  let check = typecheck type_map fun_types var_env in
  match e with
  | True -> e
  | False -> e
  | FloatLiteral n -> e
  | IntLiteral n -> e
  | StringLiteral s -> e
  | ArrayLiteral (elements, _) ->
     let elements' = map check elements in
     let t = type_of (last elements') in
     if for_all ((=) t) (map type_of elements') then
       ArrayLiteral (elements', t)
     else raise (Error "not all array elements have same type")
     
  | StructLiteral (members, _) ->
     let members' = sort (fun a b -> compare (fst a) (fst b)) members in
     let members'' = map (fun (n, rhs) -> (n, check rhs)) members' in
     let members''' = map (fun (n, rhs) -> (n, type_of rhs)) members'' in
     StructLiteral (members'', Struct members''')

  | Mem (expr, name, _) ->
     let expr' = check expr in
     begin
       match type_of expr' with
       | Struct members ->
          let t = assoc name members in
          Mem (expr', name, resolve type_map t)
       | _ -> raise (Error "no struct type")
     end

  | MemSet (lhs, name, rhs, _) ->
     let lhs' = check lhs in
     let rhs' = check rhs in
     begin
       match type_of lhs' with
       | Struct members ->
          let t = assoc name members in
          MemSet (lhs', name, rhs', resolve type_map t)
       | _ -> raise (Error "no struct type")
     end
     
  | New (expr, t) -> 
     let expr' = check expr in
     if type_of expr' = Int then
       New(expr', resolve type_map t)
     else raise (Error "new size must be int")

  | Let (name, expr, _) ->
     if Hashtbl.mem var_env name then
       raise (Error ("reassigned variable " ^ name))
     else 
       let expr' = check expr in
       let t = resolve type_map (type_of expr') in
       Hashtbl.add var_env name t;
       Let (name, expr', t)

  | Call ("[]", [array; index], _) ->
     let array' = check array in
     let index' = check index in
     if type_of index' = Int then
       match type_of array' with
       | Array t ->
          let t' = resolve type_map t in
          Call ("[]", [array'; index'], t')
       | _ -> raise (Error "left hand side is not an array")
     else raise (Error "index is not an integer")

  | Call ("[]=", [array; index; expr], _) ->
     let array' = check array in
     let index' = check index in
     let expr' = check expr in
     if type_of index' = Int then
       match type_of array' with
       | Array t ->
          let t' = resolve type_map t in
          if type_of expr' = t' then
            Call ("[]=", [array'; index'; expr'], t')
          else raise (Error "right hand side has wrong type")
       | _ -> raise (Error "left hand side is not an array")
     else raise (Error "index is not an integer")

  | Call (name, args, _) ->
     let args' = map check args in
     let arg_types = map type_of args' in
     let (name', ret_type) = lookup_function fun_types name arg_types in
     Call (name', args', resolve type_map ret_type)

  | If (cond, then_clause, else_clause, _) ->
     let cond' = check cond in
     let then_clause' = map check then_clause in
     let else_clause' = map check else_clause in
     if type_of cond' = Bool then
       if type_of (last then_clause') = type_of (last else_clause) then
         If (cond', then_clause', else_clause', type_of (last then_clause'))
       else raise (Error "then clause has not same type as else clause")
     else raise (Error "condition is not a bool")

  (* | Fun (args, types, body, _) -> *)
  (*    let env' = assign_args args types env in *)
  (*    let body' = map check body in *)
  (*    let ret_type = type_of (last body') in *)
  (*    Fun (args, types, body', Function (types, ret_type)) *)
     
  | Var (name, _) ->
     let _type = lookup_variable var_env name in
     Var (name, _type)
         
let define_struct type_map name members =          
  let members' = sort (fun a b -> compare (fst a) (fst b)) members in
  let members'' = map (fun (n, t) -> (n, resolve type_map t)) members' in
  Hashtbl.add type_map name (Struct members'')

let define_function type_map fun_types name args types body =
  let var_env = var_env args types in
  let body' = map (typecheck type_map fun_types var_env) body in
  let ret_type = type_of (last body') in
  Hashtbl.add fun_types (name, types) (name, ret_type);
  (ret_type, body')
