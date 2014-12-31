open Llvm
open Llvm_executionengine

let rec main_loop m engine code_env type_env lexbuf =
  let wrapper body =
    Ast.Fun ("", [], [], body, Types.type_of (Types.last body), false) in

  let prompt = fun () ->
    print_string "ready> "; flush stdout;
    main_loop m engine code_env type_env lexbuf in

  try match Parser.toplevel Lexer.token lexbuf with
  | Ast.End -> ()
  | Ast.Sep ->
    Lexing.flush_input lexbuf;
    main_loop m engine code_env type_env lexbuf

  | Ast.Expression e ->
    let e' = Types.typecheck type_env e in
    let t = Types.type_of e' in
    print_string ((Types.string_of_type t) ^ ": ");

    match e with
    | Ast.Fun (_,_,_,_,_,_) ->
       let f = Codegen.generate m code_env e' in
       dump_value f;
       prompt ();
    | _ ->
       let f = Codegen.generate m code_env (wrapper [e']) in
       let res = ExecutionEngine.run_function f [||] engine in

       (* dump_value f; *)

       begin
         match t with
         | Ast.Int32 -> print_int (GenericValue.as_int res)
         | Ast.Int -> print_int (GenericValue.as_int res)
         | Ast.Float -> print_float (GenericValue.as_float (Codegen.llvm_type_for t) res);
         | _ -> print_endline "unknown type"
       end;
       
       print_endline "";
       prompt ();

  with
  | Parsing.Parse_error ->
    (* Discard buffer contents for error recovery. *)
    Lexing.flush_input lexbuf; prompt ()
