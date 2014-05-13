(*****************************************************************************)
type name = string
type typ = Typ.t
type value = Value.t
type t =
    Wild
  | Name of name
  | Typed of name*typ
  | Tuple of t list
type substitution = (name*value) list

val init : name -> t
val compare : t -> t -> int
val display : bool -> t -> string
val displays : bool -> t list -> string
val to_graph : t -> string
val to_graphs : t list -> string
val to_string : t -> string
val to_html : t -> string
val free_names : t -> name list
val bind : (name*name) list -> t -> t
val bindt : name -> typ -> t -> t
val bind_value : value -> t -> substitution
val bind_values : value list -> t list -> substitution
val get_type : t -> typ
val bind_type : typ -> t -> (name*typ) list
val bind_types : typ list -> t list -> (name*typ) list
(*****************************************************************************)

