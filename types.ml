open List
open Ast
       
exception Error of string

let last list = nth list ((length list) - 1)
let type_map:(string, type_name) Hashtbl.t = Hashtbl.create 10

let assign_args args types env =
  let env' = Hashtbl.copy env in
  iter2 (fun name t -> Hashtbl.add env' (name, []) (name, t)) args types;
  env'
                                                                                 
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
  | TypeRef name -> name
  | Struct members -> "struct (" ^ String.concat "," (map (fun (name, t) -> (string_of_type t) ^ " " ^ name) members) ^ ")"
  | Pointer t -> string_of_type t ^ "*"
  | Function (args, t) -> string_of_type t ^ "(" ^ String.concat "," (map string_of_type args) ^ ")"
  | Undefined -> "undefined"

let string_of_types list = "(" ^ String.concat "," (map string_of_type list) ^ ")"

let lookup env name types =
  (try Hashtbl.find env (name, types) with
   | Not_found -> raise (Error ("could not find symbol " ^ name ^ (string_of_types types))))

let resolve = function
  | TypeRef name -> 
     (try Hashtbl.find type_map name with
      | Not_found -> raise (Error ("could not find type " ^ name)))
  | _ as t -> t

let type_of = function
  | FloatLiteral _ -> Float
  | IntLiteral _ -> Int
  | StringLiteral _ -> String
  | StructLiteral (_, t) -> resolve t
  | Call (_, _, t) -> resolve t
  | Let (_, _, t) -> resolve t
  | If (_, _, _, t) -> resolve t
  | Var (_, t) -> resolve t
  | Mem (_, _, t) -> resolve t
  | MemSet (_, _, _, t) -> resolve t
  | New (_, t) -> Pointer (resolve t)

let rec typecheck env e =
  match e with
  | FloatLiteral n -> e
  | IntLiteral n -> e
  | StringLiteral s -> e
  | StructLiteral (members, _) ->
     let members' = sort (fun a b -> compare (fst a) (fst b)) members in
     let members'' = map (fun (n, rhs) -> (n, typecheck env rhs)) members' in
     let members''' = map (fun (n, rhs) -> (n, type_of rhs)) members'' in
     StructLiteral (members'', Struct members''')

  | Mem (expr, name, _) ->
     let expr' = typecheck env expr in
     begin
       match type_of expr' with
       | Struct members ->
          let t = assoc name members in
          Mem (expr', name, resolve t)
       | _ -> raise (Error "no struct type")
     end

  | MemSet (lhs, name, rhs, _) ->
     let lhs' = typecheck env lhs in
     let rhs' = typecheck env rhs in
     begin
       match type_of lhs' with
       | Struct members ->
          let t = assoc name members in
          MemSet (lhs', name, rhs', resolve t)
       | _ -> raise (Error "no struct type")
     end
     
  | New (expr, t) -> 
     let expr' = typecheck env expr in
     if type_of expr' = Int then
       New(expr', resolve t)
     else raise (Error "new size must be int")

  | Let (name, expr, _) ->
     if Hashtbl.mem env (name, []) then
       raise (Error ("reassigned variable " ^ name))
     else 
       let expr' = typecheck env expr in
       let t = type_of expr' in
       Hashtbl.add env (name, []) (name, t);
       Let (name, expr', t)

  | Call ("[]", [array; index], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     if type_of index' = Int then
       match type_of array' with
       | Pointer t ->
          Call ("[]", [array'; index'], t)
       | _ -> raise (Error "reference is not a pointer")
     else raise (Error "index is not an integer")

  | Call ("[]=", [array; index; expr], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     let expr' = typecheck env expr in
     if type_of index' = Int then
       match type_of array' with
       | Pointer t ->
          if type_of expr' = t then
            Call ("[]=", [array'; index'; expr'], t)
          else raise (Error "right hand side has wrong type")
       | _ -> raise (Error "reference is not a pointer")
     else raise (Error "index is not an integer")

  | Call (name, args, _) ->
     let args' = map (typecheck env) args in
     let arg_types = map type_of args' in
     let (name', ret_type) = lookup env name arg_types in
     Call (name', args', ret_type)

  | If (cond, then_clause, else_clause, _) ->
     let cond' = typecheck env cond in
     let then_clause' = map (typecheck env) then_clause in
     let else_clause' = map (typecheck env) else_clause in
     if type_of cond' = Bool then
       if type_of (last then_clause') = type_of (last else_clause) then
         If (cond', then_clause', else_clause', type_of (last then_clause'))
       else raise (Error "then clause has not same type as else clause")
     else raise (Error "condition is not a bool")
     
  | Var (name, _) ->
     let (name', t) = lookup env name [] in
     Var (name', t)
         
let define_struct name members =          
  let members' = sort (fun a b -> compare (fst a) (fst b)) members in
  let members'' = map (fun (n, t) -> (n, resolve t)) members' in
  Hashtbl.add type_map name (Struct members'')

let define_function env name args types body =
  let env' = assign_args args types env in
  let body' = map (typecheck env') body in
  let ret_type = type_of (last body') in
  Hashtbl.add env (name, types) (name, ret_type);
  (ret_type, body')
