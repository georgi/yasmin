open Llvm
open Llvm_executionengine
       
let main () =
  ignore (initialize_native_target ());
  print_string "ready> "; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
  Toplevel.main_loop lexbuf;
;;

main ()
