open Sys
open Unix
open Pconfig

(* TOC:
 * Random_Utilities
 * Log_File_Utilities
 * Process_Management
 * Application_Startup
 * Client_Management
 * Run_Loop
 * Main
 *)

module FdOrderable =
  struct
    type t = Unix.file_descr
    let compare = compare
  end

module FdMap = Map.Make (FdOrderable)

exception Start_shutdown ;;

(* TODO - May prefer this method than sleep waiting *)
exception Child_stopped of int * process_status ;;

type run_state =
    RsStartingUp
  | RsRunning
  | RsShuttingDown
  | RsStopping
  ;;

let sig_mask = [sigint, sigterm, sigchld];;

type app_options = {
  mutable opt_daemonize:bool;
  mutable opt_client:bool;
  mutable opt_client_path:string;
  mutable opt_config_path:string;
}

type managed_process = {
  mp_name:string;
  mp_pid:int;
  mp_start_time:float;
  mp_command:string;
  mutable mp_deleted:bool;
  mutable mp_term_sent:float;
}

type controller_client = {
  cc_socket: file_descr;
  cc_addr: sockaddr;
  mutable cc_current_message: string;
}

type prog_state = {
  mutable ps_name:string;
  mutable ps_control_path:string;
  mutable ps_working_dir:string;
  mutable ps_run_state:run_state;
  mutable ps_log_file:string;
  mutable ps_log_fd: file_descr;
  mutable ps_monitor_list: (string * string) list;
  mutable ps_startup_list: (string * string) list;
  mutable ps_shutdown_list: (string * string) list;
  mutable ps_monitor_processes: managed_process list;
  mutable ps_control_socket: file_descr;
  (* mutable ps_client_list: controller_client list; *)
  mutable ps_client_map: controller_client FdMap.t;
}

(*****************************************************************************
 *
 * Log_File_Utilities
 *
 *)
let open_logfile filename =
  openfile filename [O_WRONLY; O_CREAT; O_APPEND] 0o666;;

let log_write logfd message =
  let msglen = String.length message in
  let rec aux m written =
    let off = written 
    and len = msglen - written in
      let n = written + (write logfd message off len) in
        if n = msglen then
          ()
        else
          aux m n
  in
  aux message 0;;

let sig_handler signo = 
  match signo with
    sigterm -> raise Start_shutdown
  (* | sigchld ->
   *   let pid, stat = waitpid [] -1
   *   raise Child_stopped (
   *)

(*****************************************************************************
 *
 * Random_Utilities
 *
 *)

let build_exec_path cwd cmd =
  Filename.concat cwd cmd;;

let split_words s =
  let rec skip_blanks i =
    if i < String.length s && s.[i] = ' ' then
      skip_blanks (i+1)
    else i in
  let rec split start i =
    if i >= String.length s then
      [String.sub s start (i-start)]
    else
      if s.[i] = ' ' then
        let j = skip_blanks i in
        String.sub s start (i-start) :: split j j
      else
        split start (i+1)
  in
  Array.of_list (split 0 0);;

(*****************************************************************************
 *
 * Process_Management
 *
 *)

let start_process app_state name command =
  try
    log_write app_state.ps_log_fd
              (Printf.sprintf "%s: Starting \"%s\"\n" name command);
    let cmd = split_words command in
    let exe = (Array.get cmd 0) in
    match fork () with
      0   -> execvp exe cmd
    | pid -> {mp_name = name;
              mp_pid = pid;
              mp_start_time = Unix.time ();
              mp_command = command;
              mp_deleted = false;
              mp_term_sent = 0.0}
  with Unix_error(errno, fname, arg) ->
    Printf.printf "%s: %s(%s) - %s\n"
        Sys.argv.(0) fname arg (error_message errno);
    exit 1;;

let start_monitor_processes app_state =
  let rec inner app_state monitor_list proc_list =
    match monitor_list with
      [] -> proc_list
    | (name, command) :: tl ->
      let proc = start_process app_state name command in
      inner app_state tl (proc :: proc_list)
  in
  inner app_state app_state.ps_monitor_list []
  ;;

let run_once app_state proc_list =
  let rec run_procs procs =
    match procs with
      [] -> ()
    | (name, command) :: tl ->
      let proc = start_process app_state name command in
        ignore (waitpid [] proc.mp_pid);
        run_procs tl
  in
    run_procs proc_list
  ;;

let restart_process app_state proc =
  let rs = app_state.ps_run_state in
  let del = proc.mp_deleted in
  match (rs, del) with
    (RsShuttingDown, _) -> None
  | (_, true) -> None
  | (_, false) -> Some (start_process app_state proc.mp_name proc.mp_command)
  ;;

let send_kill proc = 
  if proc.mp_term_sent = 0.0 then
    kill proc.mp_pid sigterm
  else if (Unix.time () -. proc.mp_term_sent) > 10.0 then
    kill proc.mp_pid sigkill
  else ()

let string_of_status ps =
  match ps with
    WEXITED code     -> Printf.sprintf "exited with code (%d)" code
  | WSIGNALED signal -> Printf.sprintf "killed with (%d)" signal
  | WSTOPPED signal  -> "stopped"

let manage_process app_state proc =
  if app_state.ps_run_state = RsShuttingDown then
    send_kill proc
  else () ;
  try
    match waitpid [WNOHANG] proc.mp_pid with
      (0, _) -> Some proc
    | (pid, stat) -> 
      let reason = string_of_status stat in
      log_write app_state.ps_log_fd (Printf.sprintf
          "%s: %s\n" proc.mp_name reason);
      restart_process app_state proc
  with Unix_error (ECHILD, _, _) ->
    log_write app_state.ps_log_fd (Printf.sprintf
              "%s: is gone \n" proc.mp_name);
    None
  ;;

let manage_processes app_state = 
  let rec aux app_state proc_list updated =
    match proc_list with
      [] -> updated
    | entry::tl ->
      match manage_process app_state entry with
        None -> aux app_state tl updated
      | Some update ->
        aux app_state tl (update :: updated)
  in
  aux app_state app_state.ps_monitor_processes []
  ;;

(*****************************************************************************
 *
 * Application_Startup
 *
 *)

let daemonize umask work_dir outfile = 
  match fork () with 
    0 -> 
      begin
         ignore (setsid ());
         match fork () with
           0 -> chdir work_dir;
                ignore (Unix.umask umask);
                close Unix.stdin;
                close Unix.stdout;
                close Unix.stderr;
                ignore (openfile "/dev/null" [O_RDWR] 0o666);
                ignore (openfile outfile [O_WRONLY; O_APPEND; O_CREAT] 0o666);
                dup2 Unix.stdout Unix.stderr; ()
                (* would also write pid file here *)
         | _ -> exit 0
      end
  | _ -> exit 0

let init_app work_dir config_list options =
  let app_state = { ps_name = "";
                    ps_control_path = "/tmp/.phnxd_control_";
                    ps_working_dir = work_dir;
                    ps_run_state = RsStartingUp;
                    ps_log_file = "";
                    ps_log_fd = stdout;
                    ps_monitor_list = [];
                    ps_startup_list = [];
                    ps_shutdown_list = [];
                    ps_monitor_processes = [];
                    ps_control_socket = socket PF_UNIX SOCK_STREAM 0;
                    ps_client_map = FdMap.empty} in
  let rec inner app_state conf =
    match conf with
      [] -> ()
    | CeName name :: tl ->
      app_state.ps_name <- name;
      app_state.ps_control_path <-
          (Printf.sprintf "/tmp/.phnxd_control_%s" name);
      inner app_state tl 
    | CeLogfile logfile :: tl ->
      app_state.ps_log_file <- logfile;
      inner app_state tl 
    | CeMonitor (name, value) :: tl ->
      app_state.ps_monitor_list
        <- (name, value) :: app_state.ps_monitor_list;
      inner app_state tl 
    | CeStartup (name, value) :: tl ->
      app_state.ps_startup_list
        <- (name, value) :: app_state.ps_startup_list;
      inner app_state tl 
    | CeShutdown (name, value) :: tl ->
      app_state.ps_shutdown_list
        <- (name, value) :: app_state.ps_shutdown_list;
      inner app_state tl 
    | CeSetenv (name, value) :: tl ->
      putenv name value;
      inner app_state tl;
  in
    inner app_state config_list;
    if options.opt_daemonize then
      daemonize 0o000 app_state.ps_working_dir app_state.ps_log_file;
    set_signal sigpipe (Signal_ignore);
    set_signal sighup  (Signal_ignore);
    set_signal sigint  (Signal_handle sig_handler);
    set_signal sigterm (Signal_handle sig_handler);
    (* set_signal sigchld (Signal_handle sig_handler); *)
    setsockopt app_state.ps_control_socket SO_REUSEADDR true;
    bind app_state.ps_control_socket (ADDR_UNIX app_state.ps_control_path);
    listen app_state.ps_control_socket 5;
    app_state

(*****************************************************************************
 *
 * Client_Management
 *
 *)

let remove_client app_state key client =
  app_state.ps_client_map <-
    FdMap.remove client.cc_socket app_state.ps_client_map;
  close client.cc_socket

let add_new_monitor_proc app_state name command =
  app_state.ps_monitor_list
    <- (name, command) :: app_state.ps_monitor_list;
  app_state.ps_monitor_processes <-
    (start_process app_state name command) :: app_state.ps_monitor_processes

let rec mark_deleted monitor_list name =
  match monitor_list with
    [] -> ()
  | hd :: tl ->
      if hd.mp_name = name then hd.mp_deleted <- true;
      mark_deleted tl name
  ;;

let delete_proc app_state name =
  let flt_fun = (fun (n, c) -> if n <> name then true else false) in
  app_state.ps_monitor_list <-
    List.filter flt_fun app_state.ps_monitor_list;
  app_state.ps_startup_list <-
    List.filter flt_fun app_state.ps_startup_list;
  app_state.ps_shutdown_list <-
    List.filter flt_fun app_state.ps_shutdown_list;
  mark_deleted app_state.ps_monitor_processes name
  ;;

let rec kill_processes app_state name monitor_list =
  match monitor_list with
    [] -> ()
  | proc :: tl ->
      if name = proc.mp_name then send_kill proc ;
      kill_processes app_state name tl
  ;;

let send_ok client =
  let msg = "ok\n" in
  ignore (write client.cc_socket msg 0 (String.length msg))

let send_status app_state client =
  let rec aux proc_list client =
    match proc_list with
      [] -> ()
    | hd::tl -> 
        let now = Unix.time () in
        let message = (Printf.sprintf "%s(%d): %f\n"
                        hd.mp_name hd.mp_pid (now -. hd.mp_start_time)) in
        ignore (write client.cc_socket message 0 (String.length message))
  in
  aux app_state.ps_monitor_processes client

let process_message app_state client message =
  try 
    let lex = (command_lexer (Stream.of_string message)) in
      match parse_command lex with
        CmdStop -> (app_state.ps_run_state <- RsStopping; send_ok client)
      | CmdStart -> (app_state.ps_run_state <- RsRunning; send_ok client)
      | CmdStatus -> send_status app_state client
      | CmdHalt -> app_state.ps_run_state <- RsShuttingDown
      | CmdNewStartup (name, command) -> ()
      | CmdNewMonitor (name, command) ->
        add_new_monitor_proc app_state name command; send_ok client
      | CmdNewShutdown (name, command) -> ()
      | CmdDelete name -> delete_proc app_state name; send_ok client
      | CmdKill name ->
          kill_processes app_state name app_state.ps_monitor_processes;
          send_ok client
  with _ -> 
    log_write app_state.ps_log_fd
        (Printf.sprintf "error processing command \"%s\"\n" message)
  ;;

let process_client_bytes app_state client =
  let buffer = String.create 4096 in
  let nread = read client.cc_socket buffer 0 4096 in
  let input = String.sub buffer 0 nread in
  let consumer = process_message app_state client in 
    client.cc_current_message <- 
      StrUtils.line_reader client.cc_current_message input consumer

let rec process_ready_ifds app_state ready_ifds =
  match ready_ifds with
    [] -> ()
  | fd :: tl ->
    if fd = app_state.ps_control_socket then
      (* new session *)
      let (csock, caddr) = accept fd in
      let cclient = {cc_socket = csock; cc_addr = caddr; 
                     cc_current_message = ""} in
      app_state.ps_client_map
        <- FdMap.add csock cclient app_state.ps_client_map;
      process_ready_ifds app_state tl
    else
      (* session input *)
      begin
        let cclient = FdMap.find fd app_state.ps_client_map in 
        try
          process_client_bytes app_state cclient
        with End_of_file ->
          (app_state.ps_client_map <-
            FdMap.remove fd app_state.ps_client_map;
            close fd);
        process_ready_ifds app_state tl
      end
  ;;

(*****************************************************************************
 *
 * Run_Loop
 *
 *)

let build_ifds_list app_state =
  let client_ifds = ref [] in
  FdMap.iter (fun k v -> client_ifds := (v.cc_socket :: !client_ifds)) 
             app_state.ps_client_map;
  app_state.ps_control_socket :: !client_ifds
  ;;

let process_inputs app_state =
  let input_list = build_ifds_list app_state in
  match select input_list [] [] 1.0 with
    ([], [], []) -> ()
  | (ready_ifds, [], []) -> process_ready_ifds app_state ready_ifds
  | _ -> ()
  ;;

let close_client key client =
  close client.cc_socket

let rec run app_state =
  try
    process_inputs app_state;
    let processes = manage_processes app_state in
    let rs = app_state.ps_run_state in
    app_state.ps_monitor_processes <- processes;
    match (rs, processes) with
      (RsShuttingDown, []) -> ()
    | (RsShuttingDown, processes) ->
        run app_state
    | (_, []) ->
        run app_state
    | (_, processes) ->
        run app_state
  with Start_shutdown ->
    app_state.ps_run_state <- RsShuttingDown;
    run app_state

let start_server_process options =
  let config = parse_config options.opt_config_path in
  let app_state = init_app (getcwd ()) config options in
    print_config config;
    run_once app_state app_state.ps_startup_list;
    app_state.ps_run_state <- RsRunning;
    app_state.ps_monitor_processes
      <- start_monitor_processes app_state;
    run app_state;
    run_once app_state app_state.ps_shutdown_list;
    FdMap.iter close_client app_state.ps_client_map;
    close app_state.ps_control_socket;
    unlink app_state.ps_control_path;
    log_write app_state.ps_log_fd "Shutdown complete\n"
  ;;

(*****************************************************************************
 *
 * Main
 *
 *)

let option_daemonize options () =
  options.opt_daemonize <- true
  ;;

let option_start_client options path =
  options.opt_client <- true;
  options.opt_client_path <- path
  ;;

let option_config options config_path =
  options.opt_config_path <- config_path
  ;;

let usage = "pnhxd [-d] <config file>";;

let parse_options () =
  let options = { opt_daemonize = false;
                  opt_client = false;
                  opt_client_path = "";
                  opt_config_path = "/etc/phnxd.conf";
                } in
  let options_spec = [
      ("-d", Arg.Unit (option_daemonize options), "Daemonize phnxd");
      ("-c", Arg.String (option_start_client options), "Start client");
    ] in 
  Arg.parse options_spec (option_config options) usage;
  options;;

let main () =
  let cmd_options = parse_options () in
  if cmd_options.opt_client then
    (handle_unix_error Client.start_client_process) cmd_options.opt_client_path
  else
    (handle_unix_error start_server_process) cmd_options
  ;;

main ();;

(* vim: set sts=2 sw=2 expandtab: *)
