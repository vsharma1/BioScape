type subrecord = Subrecord.t
type species = Species.t
type data = {counter:int; subrecord:subrecord}
type t (*F# = Tagged.Map<Species.t, data> F#*)

val empty : t
val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val plus : species -> subrecord -> t -> t
val minus : species -> t -> t

val lookup_delay : float -> t -> float * (species option)
val lookup_output : float -> t -> float * (species option)
val lookup_input : float -> float -> t -> float * ((float*species) option)
