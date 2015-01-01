open Llvm
open Llvm_executionengine
open Ast

let rec main_loop m engine lexbuf =
  let code_env:(string * type_name list, llvalue) Hashtbl.t = Hashtbl.create 10 in
  let type_env:((string * type_name list), (string * type_name)) Hashtbl.t = Hashtbl.create 10 in

  let print_res t res =
    let t' = Codegen.llvm_type_for t in
    match t with
    | Int32 -> print_int (GenericValue.as_int res)
    | Int -> print_int (GenericValue.as_int res)
    | Float -> print_float (GenericValue.as_float t' res)
    | String ->
       let puts = Codegen.lookup code_env "string_puts" [String] in
       ignore (ExecutionEngine.run_function puts (Array.of_list [res]) engine)
    | _ -> () in

  let generate_fun = function
    | Fun (_,_,_,_,_) as e ->
       let f = Codegen.generate m code_env e in
       dump_value f;
       false
    | _ -> true in

  Types.init_env type_env;
  Codegen.declare_extern m code_env type_env;

  try match Parser.toplevel Lexer.token lexbuf with
      | End -> ()
      | Sep ->
         Lexing.flush_input lexbuf;
         main_loop m engine lexbuf

      | Expression seq ->
         let seq' = List.map (Types.typecheck type_env) seq in
         let body = List.filter generate_fun seq' in
         let t = Types.type_of (Types.last body) in
         let wrap = Fun ("", [], [], body, t) in
         let f = Codegen.generate m code_env wrap in
         let res = ExecutionEngine.run_function f [||] engine in
         print_string ("=> " ^ (Types.string_of_type t) ^ ": "); flush stdout;
         print_res t res;
         print_endline "";
         main_loop m engine lexbuf

  with
  | Parsing.Parse_error ->
     Lexing.flush_input lexbuf;
     main_loop m engine lexbuf
