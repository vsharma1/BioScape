type subrecord = Subrecord.t
type species = Species.t
type imap = Imap.t
type t
val empty : t
val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val delays : t -> float
val inputs : t -> float
val outputs : t -> float
val activity : t -> float
val apparent : t -> float

(** Returns the putative time for the reaction *)
val time : t -> float
val plus : float -> float -> species -> subrecord -> t -> t
val minus : float -> float -> species -> subrecord -> t -> t

(** Updates the reaction with a new putative time step, sampled from an exponential distribution *)
val flip : t -> t
val lookup_delay : float -> t -> float * (species option)
val lookup_output : float -> t -> float * (species option)
val lookup_input : float -> t -> float * ((float*species) option)

