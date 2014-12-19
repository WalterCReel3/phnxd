open Algos

let whitespace c = 
  String.contains " \t\r\n" c;;

module StrIter = 
  struct
    type t = string
    type value_type = char
    let iter = String.iter
  end

module StrRIter =
  struct
    type t = string
    type value_type = char
    let iter f s = 
      let i = ref ((String.length s) - 1) in
      while !i >= 0 do
        f s.[!i]; decr i
      done
  end

let lstrip s p =
  let module Sf = ScalarIterableAlgo (StrIter) in
  let off = try Sf.index s (negate p) with Not_found -> 0 in
    String.sub s off ((String.length s) - off) ;;

let rstrip s p =
  let module Sf = ScalarIterableAlgo (StrRIter) in
  let roff = try Sf.index s (negate p) with Not_found -> 0 in
    String.sub s 0 ((String.length s) - roff) ;;

let strip s p =
  lstrip (rstrip s p) p;;

let index s p = 
  let module Sf = ScalarIterableAlgo (StrIter) in
  Sf.index s p

let rindex s p = 
  let module Sf = ScalarIterableAlgo (StrRIter) in
  (String.length s) - (Sf.index s p)

let line_reader sbuf buf consumer =
  let len = (String.length buf) in
  let message = ref sbuf in
  if len <> 0 then
    let i = ref 0 in
    try
      while !i <> len do
        (* loop the buf and consume possible multiple messages *)
        let lim = String.index_from buf !i '\n' in
        let slen = lim - !i in
          message := String.concat "" [!message; String.sub buf !i slen];
          consumer !message;
          i := lim + 1;
          message := ""
      done;
      !message
    with Not_found ->
      (* partial message left on the buf *)
      String.concat "" [!message; String.sub buf !i (len - !i)]
  else
    raise End_of_file
  ;;

(* vim: set sts=2 sw=2 expandtab: *)
