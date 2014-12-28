exception Error of string

let new_env:(string, Ast.type_def) Hashtbl.t = Hashtbl.create 10

let assign_env arg_defs env =
  let iter = function
    | (name, type_def) -> Hashtbl.add env name type_def in
  List.iter iter arg_defs

let lookup env name =
  (try Hashtbl.find env name with
   | Not_found -> raise (Error "unknown variable name"))

let rec typecheck e env =
  match e with
  | Ast.FloatLiteral n -> (Ast.Float, e) 
  | Ast.IntLiteral n -> (Ast.Int, e)
  | Ast.Variable (name, _) ->
     let var_type = lookup env name in
     (var_type, Ast.Variable (name, var_type))

  | Ast.Call ("+", [lhs; rhs], _) ->
     let lhs_type, lhs = typecheck lhs env in
     let rhs_type, rhs = typecheck rhs env in
     if lhs_type = rhs_type then
       (lhs_type, Ast.Call ("+", [lhs; rhs], lhs_type))
     else
       raise (Error "+ used with different types")

  | Ast.Function ((name, arg_defs, _), body) ->
    assign_env arg_defs env;
    let body_type, body = typecheck body env in
    (body_type, Ast.Function ((name, arg_defs, body_type), body))
