(*****************************************************************************)
type name = string
type value = Value.t
type pattern = Pattern.t
type species = Species.t
type typ = Typ.t
type substitution = (name*value) list
type t =
    Null
  | Parallel of t list
  | New of (value list*t)
  | Instance of (value*value list)
  | Match of (value*(value*t) list)
  | Value of (pattern*value*t)
  | Repeat of (int*t)

val display : bool -> t -> string
val to_string : t -> string
val to_html : t -> string
val free_names : t -> name list
val bind : substitution -> t -> t
val bind_pattern : value list -> pattern list -> t -> t
val to_species : t -> species list
(*****************************************************************************)
