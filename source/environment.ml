(*****************************************************************************)
type value = Value.t
type pattern = Pattern.t
type definition = Definition.t
type shape = Shape.t
type space = Value.t
(*IF-OCAML*)
module Map = Map.Make(Value)
type t = (pattern list*definition) Map.t

let find (n:value) (e:t) =
  try Some(Map.find n e)
  with Not_found -> None
(*ENDIF-OCAML*)

(*F# let Map = Map.Make(Value.compare)
type t = Tagged.Map<value,(pattern list*definition)>
let find (n:value) (e:t) = Map.tryfind n e
let map f (e:t) = Map.map f e
F#*)

let empty = Map.empty
let add (n:value) (m:pattern list) (d:definition) (e:t) =
  Map.add n (m, d) e
let display (html:bool) (e:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let newline = if html then "<br>" else "\n" in
  let f (v:value) (m,d) (s:string) =
    s ^ newline ^ Value.display html v ^ Pattern.displays html m ^
    symbol " = " ^ Definition.display html d
  in Map.fold f e ""

let to_string (e:t) = display false e
let to_html (e:t) = display true e

let to_list (e:t) =
  let f (v:value) (ms,d) acc = (v,ms,d) :: acc
  in Map.fold f e []

(*****************************************************************************)
