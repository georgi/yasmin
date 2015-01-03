open Llvm
open Llvm_executionengine
open Ast

let type_map:(string, type_name) Hashtbl.t = Hashtbl.create 10
let fun_values:(string * type_name list, llvalue) Hashtbl.t = Hashtbl.create 10
let fun_types:((string * type_name list), (string * type_name)) Hashtbl.t = Hashtbl.create 10

let rec main_loop _module engine lexbuf =
  let loop = main_loop _module engine in

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
       let puts = Codegen.lookup_function fun_values "string_puts" [String] in
       ignore (ExecutionEngine.run_function puts (Array.of_list [res]) engine)
    | _ -> () in

  Codegen.init_functions _module fun_values fun_types;

  try match Parser.toplevel Lexer.token lexbuf with
      | End -> ()
      | Sep -> Lexing.flush_input lexbuf; loop lexbuf

      | StructDef (name, members) ->
         Types.define_struct type_map name members;
         loop lexbuf
                             
      | FunDef (name, args, types, body) ->
         let (ret_type, body') = Types.define_function type_map fun_types name args types body in
         let func = Codegen.generate_function _module fun_values name args types body' ret_type in
         Hashtbl.add fun_types (name, types) (name, ret_type);
         Hashtbl.add fun_values (name, types) func;
         dump_value func;
         loop lexbuf

      | Expression body ->
         let check = Types.typecheck type_map fun_types (Types.var_env [] []) in
         let body' = List.map check body in
         let ret_type = Types.type_of (Types.last body') in
         let func = Codegen.generate_function _module fun_values "" [] [] body' ret_type in
         let res = ExecutionEngine.run_function func [||] engine in
         print_string ("=> " ^ (Types.string_of_type ret_type) ^ ": "); flush stdout;
         print_res ret_type res;
         print_endline "";
         loop lexbuf

  with
  | Types.Error e ->
     print_endline e; flush stdout;
     loop lexbuf

  | Codegen.Error e ->
     print_endline e; flush stdout;
     loop lexbuf

  | Parsing.Parse_error ->
     Lexing.flush_input lexbuf;
     loop lexbuf
