open Llvm

let main () =
  print_string "ready> "; flush stdout;
  let lexbuf = Lexing.from_channel stdin in

  Toplevel.main_loop lexbuf;

  dump_module Codegen.the_module
;;

main ()
