open List
exception Error of string

let last list = nth list ((length list) - 1)

let new_env =
  let env:((string * Ast.type_name list), Ast.type_name) Hashtbl.t = Hashtbl.create 10 in
  Hashtbl.add env ("+", [Ast.Int; Ast.Int]) Ast.Int;
  Hashtbl.add env ("+", [Ast.Float; Ast.Float]) Ast.Float;
  env

let assign_args args types env =
  let env' = Hashtbl.copy env in
  iter2 (fun name t -> Hashtbl.add env' (name, []) t) args types;
  env'

let lookup env name types =
  (try Hashtbl.find env (name, types) with
   | Not_found -> raise (Error "could not find symbol"))

let rec string_of_type = function
  | Ast.Float -> "Float"
  | Ast.Bool -> "bool"
  | Ast.Byte -> "byte"
  | Ast.Int -> "int"
  | Ast.Void -> "void"
  | Ast.Pointer t -> string_of_type t ^ "*"
  | Ast.Function (args, t) -> string_of_type t ^ "(" ^ String.concat "," (map string_of_type args) ^ ")"
  | Ast.Undefined -> "undefined"

let type_of = function
  | Ast.FloatLiteral _ -> Ast.Float
  | Ast.IntLiteral _ -> Ast.Int
  | Ast.Call (_, _, t) -> t
  | Ast.Let (_, _, _, _, t) -> t
  | Ast.Var (_, t) -> t
  | Ast.New (_, t) -> Ast.Pointer t
  | Ast.Fun (_, _, _, _, t) -> t

let rec typecheck env e =
  match e with
  | Ast.FloatLiteral n -> e
  | Ast.IntLiteral n -> e
  | Ast.New (expr, t) -> 
     let expr' = typecheck env expr in
     if type_of expr' = Ast.Int then
       Ast.New(expr', t)
     else raise (Error "new size must be int")

  | Ast.Let (t, name, expr, body, _) ->
     let expr' = typecheck env expr in
     let env' = Hashtbl.copy env in
     Hashtbl.add env' (name, []) t;
     let body' = map (typecheck env') body in
     if t = type_of expr' then
       Ast.Let (t, name, expr', body', type_of (last body'))
     else
       raise (Error "right hand side has wrong type")

  | Ast.Call ("[]", [array; index], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     if type_of index' = Ast.Int then
       match type_of array' with
       | Ast.Pointer t ->
          Ast.Call ("[]", [array'; index'], t)
       | _ -> raise (Error "reference is not a pointer")
     else raise (Error "index is not an integer")

  | Ast.Call ("[]=", [array; index; expr], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     let expr' = typecheck env expr in
     if type_of index' = Ast.Int then
       match type_of array' with
       | Ast.Pointer t ->
          if type_of expr' = t then
            Ast.Call ("[]=", [array'; index'; expr'], t)
          else raise (Error "right hand side has wrong type")
       | _ -> raise (Error "reference is not a pointer")
     else raise (Error "index is not an integer")

  | Ast.Call (name, args, _) ->
     let args' = map (typecheck env) args in
     let arg_types = map type_of args' in
     let ret_type = lookup env name arg_types in
     Ast.Call (name, args', ret_type)

  | Ast.Var (name, _) ->
     let t = lookup env name [] in
     Ast.Var (name, t)

  | Ast.Fun (name, args, types, body, ret_type) ->
     let env' = assign_args args types env in
     let body' = map (typecheck env') body in
     Hashtbl.add env (name, types) ret_type;
     Ast.Fun (name, args, types, body', ret_type)
