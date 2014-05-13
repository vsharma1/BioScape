(*
 * Fix the next reaction method (gillespie2)
*)
(*****************************************************************************)
type value = Value.t
type record = Record.t
type subrecord = Subrecord.t
type action = Action.t
type point = action*int
type species = Record.species
type substore = Substore.t
type time = float
exception Found of value*record

(*IF-OCAML*)
module Map = Map.Make(Value)
type t = record Map.t

let find (v:value) (s:t) =
  try Map.find v s
  with Not_found -> (*Record.flip*) Record.empty (* N.B. Need to initialise the time step *)
(*ENDIF-OCAML*)

(*F#
let Map = Map.Make(Value.compare)
type t = Tagged.Map<Value.t, record>

let find (v:value) (s:t) = match Map.tryfind v s with
  | Some(r) -> r
  | None -> (*Record.flip*) Record.empty (* N.B. Need to initialise the time step *)
F#*)


let empty = Map.empty

let display (html:bool) (s:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let newline = if html then "<br>" else "\n" in
  let arrow = if html then " -&gt; " else " -> " in
  let f (v:value) (r:record) (acc:string) =
    newline ^ Value.display html v ^ symbol arrow ^ Record.display html r ^ acc
  in Map.fold f s ""

let to_string (context:string) (s:t) =
  let f (v:value) (r:record) (acc:string) =
    context ^ ".store.channel," ^ Value.to_string v ^ "\n" ^
    Record.to_string (context ^ ".store.record") r ^ "\n" ^ acc
  in Map.fold f s ""
let to_html (s:t) = display true s

(* Finds a species and an index for randomly choosing an action k with equal probability *)
let lookup_delay (v:value) (index:float) (s:t) = Record.lookup_delay index (find v s)
let lookup_output (v:value) (index:float) (s:t) = Record.lookup_output index (find v s)
let lookup_input (v:value) (index:float) (s:t) = Record.lookup_input index (find v s)

let plus (time:float) (i:species) (ss:substore) (s:t) =
  let f (v:value) (sr:subrecord) (s:t) =
    let r:record = find v s in
    let r:record = Record.plus time (Value.rate v) i sr r
    in Map.add v r s
  in Substore.fold f ss s

let minus (time:float) (i:species) (ss:substore) (s:t) =
  let f (v:value) (sr:subrecord) (s:t) =
    let r:record = Record.minus time (Value.rate v) i sr (find v s) in
      if r = Record.empty (* Garbage-Collection *)
      then Map.remove v s
      else Map.add v r s
  in Substore.fold f ss s

let flip (v:value) (s:t) =
  let r:record = find v s in
  let r:record = Record.flip r
  in Map.add v r s

let gillespie2 (s:t) =
  let f (v:value) (r:record) ((min_v:value),(min_r:record),(min_time:float)) =
    if (Record.time r < min_time)
    then (v,r,Record.time r)
    else (min_v,min_r,min_time)
  in
  let (v:value),(r:record),(time:float) = Map.fold f s (Value.empty,Record.empty,infinity) in
  let index:float = Random.float (Record.activity r) (* [0..activity-1] *)
  in if time=infinity then None else Some(v,index,time)

let gillespie (s:t) =
  let (total_apparent:float) =
    let rec f (v:value) (r:record) = (+.) (Record.apparent r)
    in Map.fold f s 0.
  in
  let delay:float = (1. /. total_apparent) *. log (1. /. (Random.float 1.)) in
  let reference:float = (Random.float 1.) *. total_apparent in
  let f (v':value) (r':record) ((v:value),(r:record), (current_apparent:float)) =
    if current_apparent >= reference
    then (*F# v,r, current_apparent F#*) (*IF-OCAML*) raise (Found (v,r)) (*ENDIF-OCAML*)
    else v',r', current_apparent +. (Record.apparent r')
  in
    try
      if total_apparent = 0.
      then None
      else
	let (v:value),(r:record),(total:float) = Map.fold f s (Value.empty,Record.empty,0.)
	in Some(v,Random.float (Record.activity r),delay) (* [0..activity-1] *)
    with Found(v,r) ->  Some(v,Random.float (Record.activity r),delay) (* [0..activity-1] *)

(* need to plot actions based on the source name of the channel, rather than an exact match. *)
let plot (actions:action list) (s:t) =
  let initialise (actions:action list) =
    let f (k:action) = (k,0)
    in List.map f actions
  in
  let add (k:action) (f:float) (points:point list) =
    let f (k',i') = if k=k' then (k',i'+ (int_of_float f)) else (k',i')
    in List.map f points
  in
  let update (points:point list) (s:t) =
    let f (v:value) (r:record) (points:point list) =
      let points:point list = add (Action.Input(Value.source v,[],Value.Float 1.0)) (Record.inputs r) points in
      let points:point list = add (Action.Output(Value.source v,[],Value.Float 1.0)) (Record.outputs r) points
      in add (Action.Delay(Value.source v)) (Record.delays r) points
    in Map.fold f s points
  in
  let extract (points:point list) = List.map snd points
  in extract (update (initialise actions) s)

(*****************************************************************************)
(*

let gillespie (s:t) =
  let (apparent_rate:float) =
    let rec f (v:value) (r:record) (acc:float) = (Record.apparent r) +. acc
    in Map.fold f s 0.
  in
  let reference:float = (Random.float 1.) *. apparent_rate in
  let f (v':value) (r:record) ((v:value),(current:float)) =
    if current >= reference
    then (*F# v,current F#*) (*IF-OCAML*) raise (Found v) (*ENDIF-OCAML*)
    else v',current +. Record.apparent r
  in
  let delay:float = (1. /. apparent_rate) *. log (1. /. (Random.float 1.)) in
    try
      if apparent_rate = 0.
      then None
      else
	let (v:value),(total:float) = Map.fold f s (Value.empty,0.)
	in v,delay
    with Found(v) -> Some(v,delay)
*)
