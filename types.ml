open List
exception Error of string

let last list = nth list ((length list) - 1)

let new_env =
  let env:((string * Ast.type_def list), Ast.type_def) Hashtbl.t = Hashtbl.create 10 in
  Hashtbl.add env ("+", [Ast.Int; Ast.Int]) Ast.Int;
  Hashtbl.add env ("+", [Ast.Float; Ast.Float]) Ast.Float;
  env

let assign_args args types env =
  if length args = length types then
    iter2 (fun name t -> Hashtbl.add env (name, []) t) args types
  else raise (Error "wrong number of types")

let lookup env name types =
  (try Hashtbl.find env (name, types) with
   | Not_found -> raise (Error "could not find function with given types"))

let type_of = function
  | Ast.FloatLiteral _ -> Ast.Float
  | Ast.IntLiteral _ -> Ast.Int
  | Ast.ArrayLiteral (_, t) -> t
  | Ast.Sequence (_, t) -> t
  | Ast.Call (_, _, t) -> t
  | Ast.Def (_, _, t) -> t
  | Ast.Fun (_, _, _, t) -> t

let rec typecheck env e =
  match e with
  | Ast.FloatLiteral n -> e
  | Ast.IntLiteral n -> e

  | Ast.ArrayLiteral (list, _) ->
     let list' = map (typecheck env) list in
     let types = map type_of list' in
     if length list' > 0 then
       if for_all ((=) (hd types)) types then
         Ast.ArrayLiteral (list', Ast.Array (length types, hd types))
       else raise (Error "conflicting types in array literal")
     else
       raise (Error "empty array literal")

  | Ast.Sequence (list, _) ->
     let list' = map (typecheck env) list in
     Ast.Sequence (list', type_of (last list'))

  | Ast.Call (name, args, _) ->
     let args' = map (typecheck env) args in
     let arg_types = map type_of args' in
     let ret_type = lookup env name arg_types in
     Ast.Call (name, args', ret_type)

  | Ast.Def (name, expr, _) ->
     let expr' = typecheck env expr in
     begin
       match expr' with
       | Ast.Fun (args, types, body, ret_type) ->
          Hashtbl.add env (name, types) ret_type;
       | _ -> Hashtbl.add env (name, []) (type_of expr');
     end;
     Ast.Def(name, expr', type_of expr')

     
  | Ast.Fun (args, types, body, _) ->
     assign_args args types env;
     let body' = typecheck env body in
     Ast.Fun (args, types, body', type_of body')
