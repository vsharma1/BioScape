(*****************************************************************************)
type name = Value.name
type value = Value.t
type pattern = Pattern.t
type typ = Typ.t
type env = Env.t

type t =
  | Input of (value*pattern list*value)
  | Output of (value*value list*value)
  | Delay of (value)
  | Move of (value*value)

val display : bool -> t -> string
val to_string : string -> t -> string
val to_html : t -> string
val compare : t -> t -> int

val bind : (name*value) list -> t -> t
val bindt : name -> typ -> t -> t
val bound_names : t -> name list
val free_names : t -> name list
val eval : t -> t

(** rate k : returns the rate exponent of action k *)
val rate : t -> float

(** typecheck suffix e k : checks that action k is consistent with
environment e. Any additional bindings are added to e *)
val typecheck : string -> env -> t -> env

(** free_subject_names k : returns the list of free names that are used for output or input *)
val free_subject_names : t -> name list
 (*****************************************************************************)

