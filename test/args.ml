open Sys;;

let option_daemonize () =
  print_string "Should try a daemonize option\n"

let option_start_client path =
  print_string "Should start a client with: ";
  print_string path;
  print_newline ();;

let option_anon anon =
  print_string "Found anon arg: " ;
  print_string anon;
  print_newline ();;

let main () = 
  let options = [
    ("-d", Arg.Unit option_daemonize, "Daemonize monitor process");
    ("-c", Arg.String option_start_client, "Start client process")
  ] in
  Arg.parse options option_anon "phnxd [-d] <config file>"
  ;;

main ()
(* vim: set sts=2 sw=2 expandtab: *)
