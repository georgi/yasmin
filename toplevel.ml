open Llvm
open Llvm_executionengine
open Ast

let code_env:(string * type_name list, llvalue) Hashtbl.t = Hashtbl.create 10
let type_env:((string * type_name list), (string * type_name)) Hashtbl.t = Hashtbl.create 10

let rec main_loop m engine lexbuf =
  let print_res t res =
    let t' = Codegen.llvm_type_for t in
    match t with
    | Bool -> begin
        match GenericValue.as_int res with
        | 0 -> print_string "false"
        | _ -> print_string "true"
      end
    | Int32 -> print_int (GenericValue.as_int res)
    | Int -> print_int (GenericValue.as_int res)
    | Float -> print_float (GenericValue.as_float t' res)
    | String ->
       let puts = Codegen.lookup code_env "string_puts" [String] in
       ignore (ExecutionEngine.run_function puts (Array.of_list [res]) engine)
    | _ -> () in

  Codegen.init_env type_env;
  Codegen.declare_extern m code_env type_env;

  try match Parser.toplevel Lexer.token lexbuf with
      | End -> ()
      | Sep ->
         Lexing.flush_input lexbuf;
         main_loop m engine lexbuf

      | StructDef (name, members) ->
         Types.define_struct name members;
         main_loop m engine lexbuf
                             
      | FunDef (name, args, types, body) ->
         let (t, body') = Types.define_function type_env name args types body in
         let f = Codegen.generate_function m code_env name args types body' t in
         dump_value f;
         main_loop m engine lexbuf

      | Expression body ->
         let body' = List.map (Types.typecheck type_env) body in
         let t = Types.type_of (Types.last body') in
         let f = Codegen.generate_function m code_env "" [] [] body' t in
         let res = ExecutionEngine.run_function f [||] engine in
         print_string ("=> " ^ (Types.string_of_type t) ^ ": "); flush stdout;
         print_res t res;
         print_endline "";
         main_loop m engine lexbuf

  with
  | Types.Error e ->
     print_endline e; flush stdout;
     main_loop m engine lexbuf

  | Codegen.Error e ->
     print_endline e; flush stdout;
     main_loop m engine lexbuf

  | Parsing.Parse_error ->
     Lexing.flush_input lexbuf;
     main_loop m engine lexbuf
