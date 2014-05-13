(*
 * Instantiate types with an appropriate suffix after input typechecking.

let bind_pattern (pos:position) (t:typ) (m:pattern) (e:env) =
  let suffix:string = "~" ^ string_of_int pos.Lexing.pos_cnum in
  let t:typ = Typ.instantiate suffix t in
  let l:(name*typ) list = Pattern.bind_type "" t m
  in Env.add_list l e

*)
(*****************************************************************************)
type name = Value.name
type value = Value.t
type pattern = Pattern.t
type typ = Typ.t
type substitution = (name*value) list
type env = Env.t

type t =
  | Input of (value*pattern list*value)
  | Output of (value*value list*value)
  | Delay of (value)
  | Move of (value*value)

let display (html:bool) (k:t) =
  let symbol (s:string) = if html then "<font color=#990000>" ^ s ^"</font>" else s in
  let exponent (r:value) = if Value.rate r = 1.0 then "" else symbol "*" ^ Value.display html r
  in match k with
      | Output(v,[],r) -> symbol "!" ^ Value.display html v ^ exponent r
      | Output(v,vs,r) -> symbol "!" ^ Value.display html v ^ Value.displays html vs ^ exponent r
      | Input(v,[],r) -> symbol "?" ^ Value.display html v ^ exponent r
      | Input(v,ms,r) -> symbol "?" ^ Value.display html v ^ Pattern.displays html ms ^ exponent r
      | Delay(r) -> (*symbol "@" ^*) Value.display html r
      | Move(v,r) -> symbol "mov." ^ Value.display html v ^ symbol "@" ^ Value.display html r


let to_string (context:string) (k:t) =
  let exponent (r:value) = if Value.rate r = 1.0 then "" else "*" ^ Value.display false r
  in match k with
      | Output(v,[],r) -> "!" ^ Value.display false v ^ exponent r
      | Output(v,vs,r) -> "!" ^ Value.display false v ^ Value.displays false vs ^ exponent r
      | Input(v,[],r) -> "?" ^ Value.display false v ^ exponent r
      | Input(v,ms,r) -> "?" ^ Value.display false v ^ Pattern.displays false ms ^ exponent r
      | Delay(r) -> (*"@" ^*) Value.display false r
      | Move(v,r) -> "mov." ^ Value.display false v ^ "@" ^ Value.display false r
let to_html (k:t) = display true k

(* do not compare output value or input pattern *)
let compare (k:t) (k':t) = match k,k' with
  | Input(v,_,_),Input(v',_,_) -> Value.compare v v'
  | Output(v,_,_),Output(v',_,_) -> Value.compare v v'
  | _,_ -> compare k k'

let rate (k:t) = match k with
  | Input(_,_,r) -> Value.rate r
  | Output(_,_,r) -> Value.rate r
  | Delay(_) -> 1.0
  | Move(_,_) -> 1.0

let rec bind (s:substitution) (k:t) = match k with
  | Input(v,ms,r) -> Input(Value.bind s v,ms,Value.bind s r)
  | Output(v,vs,r) -> Output(Value.bind s v,List.map (Value.bind s) vs,Value.bind s r)
  | Delay(r) -> Delay(Value.bind s r)
  | Move(v,r) -> Move(Value.bind s v, Value.bind s r)

let rec bindt (n:name) (t:typ) (k:t) = match k with
  | Input(v,ms,r) -> Input(v,List.map (Pattern.bindt n t) ms,r)
  | _ -> k

let bound_names (k:t) =  match k with
  | Input(v,ms,r) -> List.flatten (List.map Pattern.free_names ms)
  | _ -> []

let free_names (k:t) = match k with
  | Input(v,ms,r) -> (Value.free_names v) @ (Value.free_names r)
  | Output(v,vs,r) -> (Value.free_names v) @ (List.flatten (List.map Value.free_names vs)) @ (Value.free_names r)
  | Delay(r) -> Value.free_names r
  | Move(v,r) -> (Value.free_names v) @ (Value.free_names r)

let free_subject_names (k:t) = match k with
  | Input(v,ms,r) -> Value.free_names v
  | Output(v,vs,r) -> Value.free_names v
  | Delay(r) -> []
  | Move(v,r) -> (Value.free_names v) @ (Value.free_names r)

let eval (k:t) = match k with
  | Input(v,ms,r) -> Input(Value.eval v,ms,Value.eval r)
  | Output(v,vs,r) -> Output(Value.eval v,List.map Value.eval vs,Value.eval r)
  | Delay(r) -> Delay(Value.eval r)
  | Move(v,r) -> Move(Value.eval v, Value.eval r)

let typecheck (suffix:string) (e:env) (k:t) =
  let check_float (v:value) = match Value.typecheck e v with
    | Typ.Float -> ()
    | t -> failwith (Value.to_string v ^ " of type " ^ Typ.to_string t ^ " is not a float.")
  and get_channel_type (v:value) =  match Value.typecheck e v with
    | Typ.Channel(ts) -> ts
    | t -> failwith (Value.to_string v ^ " of type " ^ Typ.to_string t ^ " is not a channel.")
  in match k with
    | Input(v,ms,r) -> check_float r;
	let ts:typ list = get_channel_type v in
	let ts:typ list = List.map (Typ.instantiate suffix) ts in
	let bindings:(name*typ) list = Pattern.bind_types ts ms
	in Env.add_list bindings e
    | Output(v,vs,r) -> check_float r;
	let sent:typ list = List.map (Value.typecheck e) vs in
	let ts:typ list = get_channel_type v in
	let _ =
	  try Typ.subtypes sent ts
	  with Failure(s) -> failwith (
	    "Channel " ^ Value.to_string v ^
	    " expects types " ^ Typ.to_strings ts ^
	    " but is sent types " ^ Typ.to_strings sent ^ ". ")
	in e
    | Delay(r) -> check_float r; e
    | Move(v,r) -> check_float r; e

(*****************************************************************************)
