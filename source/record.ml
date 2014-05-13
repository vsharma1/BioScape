(*****************************************************************************)
type subrecord = Subrecord.t
type species = Species.t
type imap = Imap.t
type t = { (* Either delays=0 or inputs=0 and outputs=0 and mixed=0 *)
  delays:float;
  inputs:float;
  outputs:float;
  mixed:float;
  speed:float;
  step:float;
  time:float;
  imap:imap
}

let empty = {
  delays = 0.0;
  inputs = 0.0;
  outputs =  0.0;
  mixed = 0.0;
  speed = 0.0;
  step = 0.0;
  time = infinity;
  imap = Imap.empty
}

let multiply (f:float) (f':float) =
  if f = 0. || f' = 0.
  then 0.0
  else f *. f'

let init (rate:float) (delays:float) (inputs:float) (outputs:float) (mixed:float) (step:float) (time:float) (imap:imap) =
  let speed:float = multiply rate (delays +. (inputs *. outputs) -. mixed)
  in
  let time:float =
    if speed = 0.
    then infinity
    else time +. (step /. speed)
  in {
      delays = delays;
      inputs = inputs;
      outputs = outputs;
      mixed = mixed;
      speed = speed;
      step = step;
      time = time;
      imap=imap
    }
let display (html:bool) (r:t) =
  "(" ^ string_of_float r.delays ^
  "," ^ string_of_float r.inputs ^
  "," ^ string_of_float r.outputs ^
  "," ^ string_of_float r.mixed ^
  "," ^ string_of_float r.speed ^
  "," ^ string_of_float r.step ^
  "," ^ string_of_float r.time ^
  ")" ^ Imap.display html r.imap

let to_string (context:string) (r:t) =
  context ^ ".delays," ^ string_of_float r.delays ^ "\n" ^
  context ^ ".inputs," ^ string_of_float r.inputs ^ "\n" ^
  context ^ ".outputs," ^ string_of_float r.outputs ^ "\n" ^
  context ^ ".mixed," ^ string_of_float r.mixed ^ "\n" ^
  context ^ ".speed," ^ string_of_float r.speed ^ "\n" ^
  context ^ ".step," ^ string_of_float r.step ^ "\n" ^
  context ^ ".time," ^ string_of_float r.time ^ "\n" ^
  Imap.to_string context r.imap
let to_html (r:t) = display true r

let delays (r:t) = r.delays
let inputs (r:t) = r.inputs
let outputs (r:t) = r.outputs
let activity (r:t) = r.delays +. (r.inputs *. r.outputs) -. r.mixed
let apparent (r:t) = r.speed
let time (r:t) = r.time

let plus (time:float) (rate:float) (i:species) (sr:subrecord) (r:t) =
  let delays:float = r.delays +. Subrecord.delays sr in
  let inputs:float = r.inputs +. Subrecord.inputs sr in
  let outputs:float = r.outputs +. Subrecord.outputs sr in
  let mixed:float = r.mixed +. Subrecord.mixed sr in
  let im:imap = Imap.plus i sr r.imap
  in init rate delays inputs outputs mixed r.step time im

let minus (time:float) (rate:float) (i:species) (sr:subrecord) (r:t) =
  let delays:float = r.delays -. Subrecord.delays sr in
  let inputs:float = r.inputs -. Subrecord.inputs sr in
  let outputs:float = r.outputs -. Subrecord.outputs sr in
  let mixed:float = r.mixed -. Subrecord.mixed sr in
  let im:imap = Imap.minus i r.imap
  in init rate delays inputs outputs mixed r.step time im

let flip (r:t) = {r with step = log (1. /. (Random.float 1.))}
let lookup_input (index:float) (r:t) = Imap.lookup_input r.outputs index r.imap
let lookup_output (index:float) (r:t) = Imap.lookup_output index r.imap
let lookup_delay (index:float) (r:t) = Imap.lookup_delay index r.imap

(*****************************************************************************)
(*
let time:float = (r.apparent /. apparent) *. (r.time -. current_time) +. current_time
let generateOffsetDelay(offset:float) =


let generateGaussianDelay(offset:float)(multiplier:float)


let nextGaussian =
  let v1:float = 2 *. Random.float -1 in
  let v2:float = 2 *. Random.float -1 in
  let s = v1 *. v1 +. v2 *. v2 in
*)
