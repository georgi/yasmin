open Llvm
open Llvm_executionengine
       
let main () =
  ignore (initialize_native_target ());

  let lexbuf = Lexing.from_channel stdin in
  let m = create_module Codegen.context "jit" in
  let engine = ExecutionEngine.create_jit m 0 in
  let type_env = Types.new_env in
  let code_env = Codegen.new_env in

  Codegen.declare_extern m code_env type_env;

  Toplevel.main_loop m engine code_env type_env lexbuf;
;;

main ()
