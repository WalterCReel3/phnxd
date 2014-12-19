exception StopIteration;;

let negate f x =
  not (f x);;

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

    let index col pred =
      let i = ref 0 in
      let inner item =
        if pred item then
          raise StopIteration
        else
          incr i
      in
      try
        T.iter (inner) col; raise Not_found
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

(* vim: set sts=2 sw=2 expandtab: *)
