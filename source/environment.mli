(*****************************************************************************)
type value = Value.t
type pattern = Pattern.t
type definition = Definition.t
type shape = Shape.t
type space = Value.t
type t (*F# = Tagged.Map<value,(pattern list*definition)> F#*)

val display : bool -> t -> string
val to_string : t -> string
val to_html : t -> string
val to_list : t -> (value*(pattern list)*definition) list
val empty : t
val add : value -> pattern list -> definition -> t -> t
val find : value -> t -> (pattern list * definition) option
(*****************************************************************************)
