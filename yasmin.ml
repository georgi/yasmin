open Llvm
open Llvm_executionengine
       
let main () =
  Printexc.record_backtrace true;
  
  ignore (initialize_native_target ());

  let lexbuf = Lexing.from_channel stdin in
  let m = create_module Codegen.context "jit" in
  let engine = ExecutionEngine.create_jit m 0 in

  Toplevel.main_loop m engine lexbuf;
;;

main ()
