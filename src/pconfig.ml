open Genlex
open StrUtils

let command_lexer = make_lexer [
  "stop"; "start"; "halt"; "new"; "delete"; "kill";
  "status"; "startup"; "shutdown"; "monitor"];;

type command  =
  | CmdStop
  | CmdStart
  | CmdStatus
  | CmdHalt
  | CmdNewStartup of string * string
  | CmdNewMonitor of string * string
  | CmdNewShutdown of string * string
  | CmdDelete of string
  | CmdKill of string
  ;;

let rec parse_command = parser
  | [< 'Kwd "stop" >] -> CmdStop
  | [< 'Kwd "start" >] -> CmdStart
  | [< 'Kwd "status" >] -> CmdStatus
  | [< 'Kwd "halt" >] -> CmdHalt
  | [< 'Kwd "new"; e=parse_new >] -> e
  | [< 'Kwd "delete"; e=parse_delete >] -> e
  | [< 'Kwd "kill"; e=parse_kill >] -> e
and parse_new = parser
  | [< 'Kwd "startup"; e=parse_new_startup >] -> e
  | [< 'Kwd "monitor"; e=parse_new_monitor >] -> e
  | [< 'Kwd "shutdown"; e=parse_new_shutdown >] -> e
and parse_new_startup = parser
  | [< 'Ident name; 'String command >] -> CmdNewStartup (name, command)
and parse_new_monitor = parser
  | [< 'Ident name; 'String command >] -> CmdNewMonitor (name, command)
and parse_new_shutdown = parser
  | [< 'Ident name; 'String command >] -> CmdNewShutdown (name, command)
and parse_delete = parser
  | [< 'Ident name >] -> CmdDelete name
and parse_kill = parser
  | [< 'Ident name >] -> CmdKill name
  ;;

type config_entry = 
    CeName of string
  | CeLogfile of string
  | CeSetenv of string * string
  | CeStartup of string * string
  | CeShutdown of string * string
  | CeMonitor of string * string
  ;;

let config_lexer = make_lexer [
  "name"; "logfile"; "setenv"; "startup"; "monitor"; "shutdown"];;

let rec parse_line = parser 
  | [< 'Kwd "name"; e=parse_name >] -> e
  | [< 'Kwd "logfile"; e=parse_logfile >] -> e
  | [< 'Kwd "setenv" ; e=parse_setenv  >] -> e
  | [< 'Kwd "startup"; e=parse_startup >] -> e
  | [< 'Kwd "monitor"; e=parse_monitor >] -> e
  | [< 'Kwd "shutdown"; e=parse_shutdown >] -> e
and parse_name = parser
  | [< 'Ident name >] -> CeName (name)
and parse_logfile = parser
  | [< 'String name >] -> (CeLogfile name)
and parse_setenv = parser
  | [< 'Ident name; 'String value >] -> CeSetenv (name, value)
and parse_startup = parser
  | [< 'Ident name; 'String command >] -> CeStartup (name, command)
and parse_monitor = parser
  | [< 'Ident name; 'String command >] -> CeMonitor (name, command)
and parse_shutdown = parser
  | [< 'Ident name; 'String command >] -> CeShutdown (name, command)
;;

let rec print_config entries =
  match entries with
    [] -> ()
  | CeName name :: tl ->
    print_endline (Printf.sprintf "name %s" name);
    print_config tl
  | CeLogfile file :: tl ->
    print_endline (Printf.sprintf "logfile %s" file);
    print_config tl
  | CeSetenv (name, value) :: tl ->
    print_endline (Printf.sprintf "setenv %s \"%s\"" name value);
    print_config tl
  | CeStartup (name, command) :: tl ->
    print_endline (Printf.sprintf "startup %s \"%s\"" name command);
    print_config tl
  | CeMonitor (name, command) :: tl ->
    print_endline (Printf.sprintf "monitor %s \"%s\"" name command);
    print_config tl
  | CeShutdown (name, command) :: tl ->
    print_endline (Printf.sprintf "shutdown %s \"%s\"" name command);
    print_config tl
  ;;

let scrub s =
  let l = String.length s in
  let cut = try String.index s '#' with Not_found -> l in
  let line = String.sub s 0 cut in
  strip line whitespace

let parse_config filename =
  let in_file = open_in filename in
  let rec read_lines infile entries =
    try 
      let line = scrub (input_line infile) in
      if (String.length line) <> 0 then
        let lex = (config_lexer (Stream.of_string line)) in
        try
          let config_entry = parse_line lex in
          read_lines infile (config_entry :: entries)
        with _ ->
          print_endline (Printf.sprintf "Could not parse line: \"%s\"" line);
          read_lines infile entries
      else
        read_lines infile entries
    with End_of_file ->
      entries
  in
  List.rev (read_lines in_file []);;

(* vim: set sts=2 sw=2 expandtab: *)
