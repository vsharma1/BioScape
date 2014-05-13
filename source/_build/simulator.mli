type typ = Typ.t
type value = Value.t
type pattern = Pattern.t
type proc = Process.t
type term = Term.t
type element = string*bool option
type action = Action.t
type substitution = (string*value) list
type environment = Environment.t
type definition = Definition.t
type species = Species.t

(** Adds a process to a term *)
val cons : proc -> term -> term

(** Performs a single reduction step on a term. Returns the new simulation time and the corresponding term
Returns None if no reductions are possible *)
val reduce : term -> (float*term) option

(** Repeatedly reduces a term until no more reductions are possible *)
val execute : float -> int -> action list -> species list -> string -> bool -> term -> unit
