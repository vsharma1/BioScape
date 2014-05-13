type store = Store.t
type action = Store.action
type record = Store.record
type choice = Choice.t
type proc = Choice.proc
type value = Choice.value
type heap = Heap.t
type environment = Environment.t
type pattern = Environment.pattern
type definition = Environment.definition
type species = Species.t
type space = Value.t
type shape = Shape.t
type process = Process.t
type volume = Volume.t
type pos = Utils.pos
type t

val empty : t
val init : environment -> volume -> t
val init_volume : t -> t
val fresh : t -> int * t
val display : bool -> t -> string
val to_string : t -> string
val to_html : t -> string
val add : species -> choice -> t -> t
val remove_delay : value -> float -> t -> (species*value list*action*proc*t) option
val remove_output :  value -> float -> t -> (species*value list*action*proc*t) option
val remove_input : value -> float -> t -> (species*value list*action*proc*float*t) option
val gillespie : t -> (value*float*float) option
val plot : action list -> species list -> t -> int list
val plot_vol : species list -> t -> (species * (string*pos) list) list
val debug : float -> t -> string
val find : value -> t -> (pattern list*definition) option

(** Updates the term with the new time and choose a new putative time step for the given reaction value *)
val flip : float -> value -> t -> t

val get_environment : t -> environment
val get_heap : t -> heap
val get_volume : t -> volume

val react : string -> float -> species list -> process list -> float -> t ->
  (bool*t)
