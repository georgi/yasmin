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

let type_of = function
  | Ast.FloatLiteral _ -> Ast.Float
  | Ast.IntLiteral _ -> Ast.Int
  | Ast.Return e -> Ast.Void
  | Ast.Sequence (_, t) -> t
  | Ast.Call (_, _, t) -> t
  | Ast.Let (_, _, _, _, t) -> t
  | Ast.Var (_, t) -> t
  | Ast.Fun (_, _, _, _, t) -> t

let rec typecheck env e =
  match e with
  | Ast.FloatLiteral n -> e
  | Ast.IntLiteral n -> e

  | Ast.Let (t, name, expr, body, _) ->
     let expr' = typecheck env expr in
     let env' = Hashtbl.copy env in
     Hashtbl.add env' (name, []) t;
     let body' = typecheck env' body in
     if t = type_of expr' then
       Ast.Let (t, name, expr', body', type_of body')
     else
       raise (Error "right hand side has wrong type")

  | Ast.Sequence (list, _) ->
     let list' = map (typecheck env) list in
     Ast.Sequence (list', type_of (last list'))

  | Ast.Call ("[]", [array; index], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     if type_of index' = Ast.Int then
       match type_of array' with
       | Ast.Array (t, n) ->
          Ast.Call ("[]", [array'; index'], t)
       | _ -> raise (Error "reference is not an array")
     else raise (Error "index is not an integer")

  | Ast.Call ("[]=", [array; index; expr], _) ->
     let array' = typecheck env array in
     let index' = typecheck env index in
     let expr' = typecheck env expr in
     if type_of index' = Ast.Int then
       match type_of array' with
       | Ast.Array (t, n) ->
          if type_of expr' = t then
            Ast.Call ("[]=", [array'; index'; expr'], t)
          else raise (Error "right hand side has wrong type")
       | _ -> raise (Error "reference is not an array")
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
     let body' = typecheck env' body in
     Hashtbl.add env (name, types) ret_type;
     Ast.Fun (name, args, types, body', ret_type)
