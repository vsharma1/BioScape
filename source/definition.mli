(*****************************************************************************)
type value = Value.t
type proc = Process.t
type choice = Choice.t
type t =
    Process of proc
  | Choice of choice
type substitution = (string*value) list

val display : bool -> t -> string
val to_string : t -> string
val to_html : t -> string
val bind : substitution -> t -> t
(*****************************************************************************)

