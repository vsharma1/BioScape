(*****************************************************************************)
type name = string
type typ = Typ.t
(*IF-OCAML*)
module Store = Map.Make(String)
type typenv = typ Store.t
(*ENDIF-OCAML*)

(*F# let Store = Map.Make(String.compare)
type typenv = Tagged.Map<string,typ> F#*)

type t = {values:typenv;types:typenv}
let empty = {values = Store.empty;types = Store.empty}

let find (n:name) (e:t) =
  try Some(Store.find n e.values)
  with Not_found -> None

let find_type (n:name) (e:t) =
  try Some(Store.find n e.types)
  with Not_found -> None

let rec add (n:name) (ty:typ) (e:t) = match find n e with
  | Some(Typ.Process(_)) -> failwith ("Multiple definitions of process " ^ n ^ ".")
  | _ -> {e with values = Store.add n (Typ.unfold ty) e.values}

let rec add_list (l:(name*typ) list) (e:t) = match l with
    [] -> e
  | (n,ty)::l ->  add_list l (add n ty e)

let add_type (n:name) (ty:typ) (e:t) = {e with types = Store.add n (Typ.unfold ty) e.types}

(*****************************************************************************)
(* let add (n:name) (ty:typ) (e:t) = {e with values = Store.add n ty e.values} *)
