(*****************************************************************************)
type name = string
type typ = Typ.t
type t
val empty : t
val add : name -> typ -> t -> t
val add_list : (name*typ) list -> t -> t
val add_type : name -> typ -> t -> t
val find : name -> t -> typ option
val find_type : name -> t -> typ option
(*****************************************************************************)
