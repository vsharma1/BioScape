type value = Value.t
type subrecord = Subrecord.t
type t (*F# = Tagged.Map<Value.t,subrecord>  F#*)

val empty : t
val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val add_input : value -> float -> t -> t
val add_output : value -> float -> t -> t
val add_delay : value -> float -> t -> t
val fold : (value -> subrecord -> 'a -> 'a ) -> t -> 'a -> 'a
