open Llvm
open Llvm_executionengine

let rec main_loop lexbuf =
  let engine = ExecutionEngine.create Codegen.the_module in
  let type_env = Types.new_env in
  let code_env = Codegen.new_env in

  let wrapper body =
    Ast.Fun ("", [], [], Ast.Return body, Types.type_of body) in

  let prompt = fun () ->
    print_string "ready> "; flush stdout;
    main_loop lexbuf in

  try match Parser.toplevel Lexer.token lexbuf with
  | Ast.End -> ()
  | Ast.Sep ->
    Lexing.flush_input lexbuf;
    main_loop lexbuf

  | Ast.Expression e ->
    let e' = Types.typecheck type_env e in

    match e with
    | Ast.Fun (_,_,_,_,_) ->
       let f = Codegen.generate code_env e' in
       dump_value f;
       prompt ();
    | _ ->
       let t = Types.type_of e' in
       let f = Codegen.generate code_env (wrapper e') in
       let res = ExecutionEngine.run_function f [||] engine in

       (* dump_value f; *)

       begin
         match t with
         | Ast.Int -> print_int (GenericValue.as_int res)
         | Ast.Float -> print_float (GenericValue.as_float (Codegen.type_for t) res);
         | _ -> print_endline "unknown type"
       end;
       
       print_endline "";
       prompt ();

  with
  | Parsing.Parse_error ->
    (* Discard buffer contents for error recovery. *)
    Lexing.flush_input lexbuf; prompt ()
