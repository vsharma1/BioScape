(*****************************************************************************)
type name = string
type typ = Typ.t
type value = Value.t
type action = Action.t
type proc = Process.t
type substore = Substore.t
type volume = Volume.t
type substitution = (string*value) list
type t = { (* assumes that there are no inputs or outputs on restricted channels *)
  restricted: value list;
  actions: (action * proc) list
}
val init : value list -> (action*proc) list -> t
val display : bool -> t -> string
val to_string : t -> string
val to_html : t -> string
val eval : t -> t
val find_action : float -> action -> t -> (float*value list*action*proc) option
val bind : substitution -> t -> t
val create_substore : volume -> t -> (volume*substore)
val free_names : t -> name list
(** ok c : checks that there are no inputs or outputs on restricted channels*)
val ok : t -> bool
(*****************************************************************************)

