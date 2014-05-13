(*****************************************************************************)
type value = Value.t
type t = value * value list
let compare (i:t) (i':t) = compare i i'
let display (html:bool) ((v,vs):t) =
  Value.display html v ^
  Value.displays html vs
let to_string (s:t) = display false s
let source ((v,vs):t) = Value.source v, List.map Value.source vs
let id ((v,vs):t) = v
let matches ((v,vs):t) ((v',vs'):t) = if (vs) = [] then (v) = (v')
  else (v,vs) = (v',vs')
(*****************************************************************************)
