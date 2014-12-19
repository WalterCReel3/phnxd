open Unix;;

exception Server_quit

exception Client_quit

type connection = {
  con_socket: file_descr;
  con_addr: sockaddr;
  mutable con_response_message: string;
  mutable con_input_message: string;
}

(* this needs to go into a Utils module *)
let process_input_bytes current consumer fd =
  let buffer = String.create 4096 in
  let nread = read fd buffer 0 4096 in
  let input = String.sub buffer 0 nread in
    StrUtils.line_reader current input consumer
  ;;

let print_response_string resp =
  print_endline resp
  ;;

let rec send_command conn cmd =
  let msg = cmd ^ "\n" in
  ignore (write conn.con_socket msg 0 (String.length msg))
  ;;

let rec process_ready_ifds conn ready_ifds =
  match ready_ifds with
    [] -> ()
  | fd :: tl ->
    if fd = conn.con_socket then
      try
        conn.con_response_message <- 
          process_input_bytes
              conn.con_response_message 
              (print_response_string)
              conn.con_socket
      with End_of_file ->
        raise Server_quit
    else
      try
        conn.con_input_message <- 
          process_input_bytes
              conn.con_input_message 
              (send_command conn)
              Unix.stdin
      with End_of_file ->
        raise Client_quit
  ;;

let process_inputs conn =
  let input_list = [conn.con_socket; Unix.stdin] in
  match select input_list [] [] 1.0 with
    ([], [], []) -> ()
  | (ready_ifds, [], []) -> process_ready_ifds conn ready_ifds
  | _ -> ()
  ;;

let rec run conn =
  process_inputs conn;
  run conn
  ;;

let start_client_process name = 
  let sock_path = (Printf.sprintf "/tmp/.phnxd_control_%s" name) in
  let client_sock = socket PF_UNIX SOCK_STREAM 0 in
  connect client_sock (ADDR_UNIX sock_path);
  let conn = { con_socket = client_sock;
               con_addr = (ADDR_UNIX sock_path);
               con_response_message = "";
               con_input_message = "" } in
  try
    run conn
  with Server_quit ->
    print_endline "Server closed"
  |    Client_quit ->
    print_endline "Exited"
  
(* vim: set sts=2 sw=2 expandtab: *)
