(*****************************************************************************)
type position = Lexing.position
exception Error of position * string
type name = string
type t =
    Void
  | String
  | Int
  | Bool
  | Char
  | Float
  | Poly of name
  | Name of name
  | Channel of (t list)
  | Process of (t list)
  | Tuple of (t list)
  | List of t
  | Data of (name*t list) list
  | Recursive of (name*t)
  | Coordinate
  | Shape
  | Space

val display : bool -> t -> string
val to_string : t -> string
val to_strings : t list -> string
val to_html : t -> string
val bind : name -> t -> t -> t
val subtype : t -> t -> t
val subtypes : t list -> t list -> t
val instantiate : string -> t -> t
val free_names : t -> name list
val compatible : t -> t -> bool
val unfold : t -> t
(*****************************************************************************)
