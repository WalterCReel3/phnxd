open Printf
open StrUtils

let pr = print_string;;

let consumer line = 
  pr (sprintf "   -> \"%s\"\n" line)

let test_line_reader () =
  let buffer = ref "" in
  pr (sprintf "Two complete messages:\n");
  let message1 = "message one here;\nmessage two here;\n" in
  buffer := line_reader !buffer message1 consumer;
  pr (sprintf "One complete message/Two partial components:\n");
  let message2 = "message one here;\nmessage t" in
  let message3 = "wo here;\n" in
  buffer := line_reader !buffer message2 consumer;
  buffer := line_reader !buffer message3 consumer;
  pr (sprintf "Three partial components:\n");
  let message4 = "message on" in
  let message5 = "e he" in
  let message6 = "re\n" in
  buffer := line_reader !buffer message4 consumer;
  buffer := line_reader !buffer message5 consumer;
  buffer := line_reader !buffer message6 consumer

let test_stripping () =
  (* trimming a mix of whitespace characters *)
  pr (sprintf "Result should be \"hi there\": ");
  pr (sprintf "\"%s\"\n" (lstrip "   \thi there" (whitespace)));
  pr (sprintf "Result should be \"hello again\": ");
  pr (sprintf "\"%s\"\n" (rstrip "hello again  \t " (whitespace)));
  (* nothing to trim *)
  pr (sprintf "Result should be \"hi there\": ");
  pr (sprintf "\"%s\"\n" (lstrip "hi there" (whitespace)));
  pr (sprintf "Result should be \"hello again\": ");
  pr (sprintf "\"%s\"\n" (rstrip "hello again" (whitespace)));
  (* stripping from both ends *)
  pr (sprintf "Result should be \"whatever and stuff\": ");
  pr (sprintf "\"%s\"\n" (strip " \t  whatever and stuff \t  \n" (whitespace)))
  ;;
  
let test_index () =
  pr (sprintf "Result should be 5: ");
  pr (sprintf "%d\n" (index "hello there" (whitespace)))
  ;;

let main () =
  pr "Starting\n";
  test_index ();
  test_stripping ();
  test_line_reader ()
  ;;

main ();;

(* vim: set sts=2 sw=2 expandtab: *)
