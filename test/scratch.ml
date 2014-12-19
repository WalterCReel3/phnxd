open Printf

exception StopIteration;;

module type SCALAR_ITERABLE =
  sig
    type t
    type value_type
    val iter : (value_type -> unit) -> t -> unit
  end

module type RANDOM_ACCESS =
  sig
    type t
    type value_type
    val get : int -> value_type
    val set : t -> int -> value_type -> unit
    val length : t -> int
    val fill : t -> int -> int -> value_type -> unit
    val iter : (value_type -> unit) -> t -> unit
    val riter : (value_type -> unit) -> t -> unit
  end

module ScalarIterableAlgo (T : SCALAR_ITERABLE) =
  struct
    type t = T.t

    let findi col pred =
      let i = ref 0 in
      let inner item =
        if pred item then
          raise StopIteration
        else
          incr i
      in
      try
        T.iter (inner) col; -1
      with StopIteration ->
        !i;;

    let to_list col =
      let res = ref [] in
      let inner item = res := item :: !res in
      T.iter (inner) col;
      List.rev !res;;

    let exists col v =
      let inner item =
        if item = v then
          raise StopIteration
        else ()
      in
      try
        T.iter (inner) col; false
      with StopIteration ->
        true;;
  end

let whitespace c = 
  String.contains " \t\r\n" c;;

let negate f x =
  not (f x);;

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

let str_findi s p =
  let module Sf = ScalarIterableAlgo (StrIter) in
  Sf.findi s p;;

let str_lstrip s p =
  let module Sf = ScalarIterableAlgo (StrIter) in
  let res = Sf.findi s (negate p) in
  let off = if res <> -1 then res else 0 in
    String.sub s off ((String.length s) - off) ;;

let str_rstrip s p =
  let module Sf = ScalarIterableAlgo (StrRIter) in
  let l = String.length s in
  let res = Sf.findi s (negate p) in
  let roff = if res <> -1 then res else 0 in
    String.sub s 0 (l - roff) ;;

let str_strip s p =
  str_lstrip (str_rstrip s p) p;;

let pr = print_string;;

let main () =
  pr (sprintf "Result should be 5: ");
  pr (sprintf "%d\n" (str_findi "hello there" (whitespace)));
  (* trimming a mix of whitespace characters *)
  pr (sprintf "Result should be \"hi there\": ");
  pr (sprintf "\"%s\"\n" (str_lstrip "   \thi there" (whitespace)));
  pr (sprintf "Result should be \"hello again\": ");
  pr (sprintf "\"%s\"\n" (str_rstrip "hello again  \t " (whitespace)));
  (* nothing to trim *)
  pr (sprintf "Result should be \"hi there\": ");
  pr (sprintf "\"%s\"\n" (str_lstrip "hi there" (whitespace)));
  pr (sprintf "Result should be \"hello again\": ");
  pr (sprintf "\"%s\"\n" (str_rstrip "hello again" (whitespace)));
  (* stripping from both ends *)
  pr (sprintf "Result should be \"whatever and stuff\": ");
  pr (sprintf "\"%s\"\n" (str_strip " \t  whatever and stuff \t  \n"
                         (whitespace)))
  ;;

main ();;

(* vim: set sts=2 sw=2 expandtab: *)
