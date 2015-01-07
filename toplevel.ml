open Llvm
open Llvm_executionengine
open Ast

let type_map:(string, type_name) Hashtbl.t = Hashtbl.create 10
let fun_types:(string, (string * type_name * type_name list) list) Hashtbl.t = Hashtbl.create 10

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
    | Byte | Int16 | Int32 | Int -> print_int (GenericValue.as_int res)
    | Float -> print_float (GenericValue.as_float t' res)
    | Void -> print_string "void"
    | Array Byte ->
       begin
         match lookup_function "string_puts" _module with
         | Some func ->
            ignore (ExecutionEngine.run_function func (Array.of_list [res]) engine)
         | _ -> ()
       end;
    | _ -> () in

  Codegen.init_functions _module fun_types;

  try match Parser.toplevel Lexer.token lexbuf with
      | End -> ()
      | Sep ->
         Lexing.flush_input lexbuf;
         loop lexbuf

      | StructDef (name, members) ->
         Types.define_struct type_map name members;
         loop lexbuf
                             
      | FunDef (name, args, types, body) ->
         let (ret_type, body') = Types.define_function type_map fun_types name args types body in
         let body'' = Codegen.generate_lambdas _module [] body' in
         let func = Codegen.generate_function _module name args types body'' ret_type in
         Types.add_function fun_types name name ret_type types;
         dump_value func;
         loop lexbuf

      | Expression body ->
         let var_env = Types.create_var_env [] [] in
         let body' = Types.typecheck type_map fun_types var_env body in
         Types.print_expr body';
         let ret_type = Types.type_of body' in
         let body'' = Codegen.generate_lambdas _module [] body' in
         (* print_endline (Types.string_of_expr body''); flush stdout; *)
         let func = Codegen.generate_function _module "" [] [] body'' ret_type in
         (* dump_value func; *)
         let res = ExecutionEngine.run_function func [||] engine in
         print_string ("=> " ^ (Types.string_of_type ret_type) ^ ": "); flush stdout;
         print_res ret_type res; print_endline "";
         loop lexbuf

  with
  | Types.Error e ->
     print_endline e;
     Printexc.print_backtrace stdout; flush stdout;
     Lexing.flush_input lexbuf;
     loop lexbuf

  | Codegen.Error e ->
     print_endline e;
     Printexc.print_backtrace stdout; flush stdout;
     Lexing.flush_input lexbuf;
     loop lexbuf

  | Parsing.Parse_error ->
     Printexc.print_backtrace stdout;
     Lexing.flush_input lexbuf;
     loop lexbuf
