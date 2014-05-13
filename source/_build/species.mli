(*****************************************************************************)
type value = Value.t
type t = value * value list
val compare : t -> t -> int
val to_string : t -> string
val source : t -> t
val id : t -> value
val matches : t -> t -> bool
(*****************************************************************************)

