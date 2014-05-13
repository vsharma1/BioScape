type subrecord = Subrecord.t
type value = Value.t

(*IF-OCAML*)
module Map = Map.Make(Value)
type t = subrecord Map.t

let find (v:value) (s:t) =
  try Map.find v s
  with Not_found -> Subrecord.empty
(*ENDIF-OCAML*)

(*F#
let Map = Map.Make(Value.compare)
type t = Tagged.Map<Value.t,subrecord>

let find (v:value) (s:t) = match Map.tryfind v s with
  | Some(r) -> r
  | None -> Subrecord.empty
F#*)

let empty = Map.empty

let display (html:bool) (s:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let newline = if html then "<br>" else "\n" in
  let tab = if html then "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;" else "        " in
  let arrow = if html then " -&gt; " else " -> " in
  let f (v:value) (r:subrecord) (acc:string) =
    newline ^ tab ^ Value.display html v ^ symbol arrow ^ Subrecord.to_string "" r ^ acc
  in Map.fold f s ""

let to_string (context:string) (s:t) =
  let f (v:value) (r:subrecord) (acc:string) =
    context ^ ".value,"  ^ Value.to_string v ^ "\n" ^
    Subrecord.to_string (context ^ ".subrecord") r ^ acc
  in Map.fold f s ""

let to_html (s:t) = display true s

let add_input (v:value) (rate:float) (s:t) = Map.add v (Subrecord.add_input rate (find v s)) s
let add_output (v:value) (rate:float) (s:t) = Map.add v (Subrecord.add_output rate (find v s)) s
let add_delay (v:value) (rate:float) (s:t) = Map.add v (Subrecord.add_delay rate (find v s)) s

let fold f m a = Map.fold f m a

(*****************************************************************************)
