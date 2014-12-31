open List
open Ast
       
exception Error of string

let last list = nth list ((length list) - 1)

let new_env =
  let env:((string * type_name list), type_name) Hashtbl.t = Hashtbl.create 10 in
  Hashtbl.add env ("+", [Int; Int]) Int;
  Hashtbl.add env ("+", [Float; Float]) Float;
  Hashtbl.add env ("+", [Pointer Byte; Pointer Byte]) (Pointer Byte);
  env

let assign_args args types env =
  let env' = Hashtbl.copy env in
  iter2 (fun name t -> Hashtbl.add env' (name, []) t) args types;
  env'

let lookup env name types =
  (try Hashtbl.find env (name, types) with
   | Not_found -> raise (Error ("could not find symbol " ^ name)))

let rec string_of_type = function
  | Float -> "float"
  | Bool -> "bool"
  | Byte -> "byte"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int -> "int"
  | Void -> "void"
  | Pointer t -> string_of_type t ^ "*"
  | Function (args, t) -> string_of_type t ^ "(" ^ String.concat "," (map string_of_type args) ^ ")"
  | Undefined -> "undefined"

let type_of = function
  | FloatLiteral _ -> Float
  | IntLiteral _ -> Int
  | StringLiteral _ -> Pointer Byte
  | Call (_, _, t) -> t
  | Let (_, _, t) -> t
  | Var (_, t) -> t
  | New (_, t) -> Pointer t
  | Fun (_, _, _, _, t, _) -> t

let rec typecheck env e =
  match e with
  | FloatLiteral n -> e
  | IntLiteral n -> e
  | StringLiteral s -> e
                            
  | New (expr, t) -> 
     let expr' = typecheck env expr in
     if type_of expr' = Int then
       New(expr', t)
     else raise (Error "new size must be int")

  | Let (name, expr, _) ->
     if Hashtbl.mem env (name, []) then
       raise (Error ("reassigned variable " ^ name))
     else 
       let expr' = typecheck env expr in
       let t = type_of expr' in
       Hashtbl.add env (name, []) t;
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
     let ret_type = lookup env name arg_types in
     Call (name, args', ret_type)

  | Var (name, _) ->
     let t = lookup env name [] in
     Var (name, t)

  | Fun (name, args, types, body, ret_type, new_scope) ->
     let env' = assign_args args types env in
     let body' = map (typecheck env') body in
     Hashtbl.add env (name, types) ret_type;
     Fun (name, args, types, body', ret_type, new_scope)
