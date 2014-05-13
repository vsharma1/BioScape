type value = Value.t
type record = Record.t
type action = Action.t
type species = Species.t
type substore = Substore.t
type time = float
type t (*F# = Tagged.Map<Value.t, record> F#*)

val empty : t
val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val plus : time -> species -> substore -> t -> t
val minus : time -> species -> substore -> t -> t

(** gillespie t: Returns the fastest reaction that is inside the store
t, together with the activity and duration of the reaction. Direct
Method. *)
val gillespie : t -> (value*float*float) option

(** gillespie2 t: Returns the fastest reaction that is inside the
store t, together with the activity of the reaction and the current
time. Next Reaction Method. *)
val gillespie2 : t -> (value*float*float) option

(** Updates the reaction value with a new putative time step, sampled from an exponential distribution *)
val flip : value -> t -> t
val plot : action list -> t -> int list
val lookup_delay : value -> float -> t -> float * (species option)
val lookup_output : value -> float -> t -> float * (species option)
val lookup_input : value -> float -> t -> float * ((float*species) option)
