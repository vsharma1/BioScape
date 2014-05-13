type subrecord = Subrecord.t
type species = Species.t
type data = {counter:int; subrecord:subrecord}

(*IF-OCAML*)
module Map = Map.Make(Species)
type t = data Map.t

let find (i:species) (im:t) =
  try Some(Map.find i im)
  with Not_found -> None
(*ENDIF-OCAML*)

(*F#
let Map = Map.Make(Species.compare)
type t = Tagged.Map<Species.t, data>

let find (i:species) (im:t) = Map.tryfind i im
let mod_float (x:float) (y:float) = x - y * System.Math.Truncate(x/y)
F#*)

let empty = Map.empty

let display (html:bool) (im:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let newline = if html then "<br>" else "\n" in
  let tab = if html then "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" else "        " in
  let arrow = if html then " -&gt; " else " -> " in
  let f (i:species) (d:data) (acc:string) =
    newline ^ tab ^ Species.to_string i ^ symbol arrow ^
    string_of_int d.counter ^ symbol " * " ^ Subrecord.to_string "" d.subrecord ^ acc
  in Map.fold f im ""

let to_string (context:string) (im:t) =
  let f (i:species) (d:data) (acc:string) =
    context ^ ".imap.species," ^ Species.to_string i ^ "\n" ^
    context ^ ".imap.species.count," ^ string_of_int d.counter ^ "\n" ^
    Subrecord.to_string (context ^ ".imap") d.subrecord ^ acc
  in Map.fold f im ""
let to_html (im:t) = display true im

let plus (i:species) (sr:subrecord) (im:t) = match find i im with
    None -> Map.add i {counter = 1; subrecord = sr} im
  | Some(d) -> Map.add i {d with counter = d.counter+1} im

let minus (i:species) (im:t) = match find i im with
    None -> im
  | Some(d) ->
      if d.counter <= 1
      then Map.remove i im
      else Map.add i {d with counter=d.counter-1} im

let lookup (select:subrecord -> float) (index:float) (im:t) = (* index >= 0 *)
  let f (i:species) (d:data) ((index:float),(result:species option)) = match result with
      Some(i) -> index,result
    | None ->
	let frequency:float = select d.subrecord in
	let index':float = index -. ((float_of_int d.counter) *. frequency) in
	  if index' < 0.0
	  then (mod_float index frequency),Some(i)
	  else index',None
  in Map.fold f im (index,None)

let lookup_delay (index:float) (im:t) = lookup Subrecord.delays index im
let lookup_output (index:float) (im:t) = lookup Subrecord.outputs index im
let lookup_input (total_outputs:float) (index:float) (im:t) = (* index >= 0 *)
  let f (i:species) (d:data) ((index:float),(result:(float*species) option)) = match result with
	Some(_) -> index,result
      | None ->
	  let inputs:float = Subrecord.inputs d.subrecord in
	  let outputs:float = Subrecord.outputs d.subrecord in
	  let population:float = float_of_int d.counter in
	  let external_outputs:float = total_outputs -. outputs in
	  let index':float = index -. (population *. (inputs *. external_outputs)) in
	    if index' < 0.0
	    then (mod_float index (inputs *. external_outputs)) , Some(external_outputs,i)
	    else index',None
  in Map.fold f im (index,None)

(*****************************************************************************)
