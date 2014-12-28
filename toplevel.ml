(*===----------------------------------------------------------------------===
 * Top-Level parsing and JIT Driver
 *===----------------------------------------------------------------------===*)

open Llvm

(* top ::= definition | external | expression | ';' *)
let rec main_loop lexbuf =
  let prompt = fun () ->
    print_string "ready> "; flush stdout;
    main_loop lexbuf in
  try match Parser.toplevel Lexer.token lexbuf with
  | Ast.End -> ()

  (* ignore top-level semicolons. *)
  | Ast.Sep ->
    Lexing.flush_input lexbuf;
    main_loop lexbuf

  (* | Ast.Extern e -> *)
  (*   dump_value (Codegen.generate e); *)
  (*   prompt () *)

  | Ast.Expression e ->
    let t, e = (Types.typecheck e Types.new_env) in
    dump_value (Codegen.generate e Codegen.new_env);
    prompt ()

  | _ ->
    Lexing.flush_input lexbuf; prompt ()

  with
  | Parsing.Parse_error ->
    (* Discard buffer contents for error recovery. *)
    Lexing.flush_input lexbuf; prompt ()
