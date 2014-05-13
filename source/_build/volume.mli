type t
type pos = Utils.pos
type species = Species.t
type value = Value.t
type shape = Shape.t

val empty : t

val add_movement : species -> float -> t -> t
val add_spatialinfo : species -> value -> shape -> float -> t -> t

val add : species -> t -> t
val react : string -> float -> species list -> species list -> float -> t ->
  (bool*t)

val display : bool -> t -> string
val plot : species list -> t -> (species * (string*pos) list) list
val spatial_info : string -> t -> string
