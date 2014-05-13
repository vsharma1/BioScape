(*****************************************************************************)
(* Either delays=0 or inputs=0 and outputs=0 *)
type t = {
  delays:float;
  inputs:float;
  outputs:float;
}

let empty = {
  delays = 0.0;
  inputs = 0.0;
  outputs =  0.0;
}

let to_string (context:string) (r:t) =
  context ^ ".delays," ^ string_of_float r.delays ^ "\n" ^
  context ^ ".inputs," ^ string_of_float r.inputs ^ "\n" ^
  context ^ ".outputs," ^ string_of_float r.outputs ^ "\n"

let delays (r:t) = r.delays
let inputs (r:t) = r.inputs
let outputs (r:t) = r.outputs
let mixed (r:t) = r.inputs *. r.outputs

let init (delays:float) (inputs:float) (outputs:float) = {
  delays = delays;
  inputs = inputs;
  outputs = outputs;
}

let plus (r:t) (r':t) = init (r.delays +. r'.delays) (r.inputs +. r'.inputs) (r.outputs +. r'.outputs)
let minus (r:t) (r':t) = init (r.delays -. r'.delays) (r.inputs -. r'.inputs) (r.outputs -. r'.outputs)
let add_input (rate:float) (r:t) = {r with inputs = r.inputs +. rate}
let add_output (rate:float) (r:t) = {r with outputs = r.outputs +. rate}
let add_delay (rate:float) (r:t) = {r with delays = r.delays +. 1.0}
(*****************************************************************************)
