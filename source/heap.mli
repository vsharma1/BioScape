type substore = Substore.t
type choice = Choice.t
type species = Species.t
type value = Value.t
type environment = Environment.t
type t (*F# = Tagged.Map<Species.t, (int * substore * choice)>  F#*)

val empty : t
val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val fold : (species -> 'a -> 'a) -> 'a -> t -> 'a
val find : species -> t -> (int*substore*choice) option
val add : species -> (int*substore*choice) -> t -> t
val remove : species -> t -> (t*substore*choice) option
val plot : species list -> t -> int list
