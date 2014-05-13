(*****************************************************************************)
type name = string
type value = Value.t
type typ = Typ.t
type action = Action.t
type volume = Volume.t
type proc = Process.t
type substore = Substore.t
type t = { (* assumes that there are no inputs or outputs on restricted channels *)
  restricted: value list;
  actions: (action * proc) list
}
type substitution = (string*value) list

let init (vs:value list) (l:(action*proc) list) = {restricted = vs; actions = l}

let unbind (ns:name list) (s:substitution) =
  let f (s:substitution) (n:name) = List.remove_assoc n s
  in List.fold_left f s ns

let bind (s:substitution) (c:t) =
  let vs:value list = List.map (Value.bind s) c.restricted in
  let ns:name list = Value.channels vs in
  let s:substitution = unbind ns s in
  let f (k,p) = (Action.bind s k) , (Process.bind (unbind (Action.bound_names k) s) p)
  in {restricted = vs; actions = List.map f c.actions}

let create_substore (vol:volume) (c:t) =
  let f ((vol:volume),(s:substore)) (k,p) = match k with
  | Action.Input(v,ms,r) -> vol, Substore.add_input v (Value.rate (Value.eval r)) s
  | Action.Output(v,vs,r) -> vol, Substore.add_output v (Value.rate (Value.eval r)) s
  | Action.Delay(r) -> vol, Substore.add_delay r (Value.rate (Value.eval r)) s
  | Action.Move(v,r) -> Volume.add_movement (v, []) (Value.rate r) vol, s
  in List.fold_left f (vol,Substore.empty) c.actions

let rec disjoint (ns:name list) (l:name list) = match ns with
  | [] -> true
  | n::ns -> (not (List.mem n l)) && (disjoint ns l)

let ok (c:t) =
  let ns:name list = Value.channels c.restricted in
  let f (k,p) (b:bool) = (disjoint (Action.free_subject_names k) ns) && b
  in List.fold_right f c.actions true

let rec remove_duplicates (ns:name list) = match ns with
  | [] -> []
  | n::ns -> if List.mem n ns then remove_duplicates ns else n::(remove_duplicates ns)

let free_names (c:t) =
  let minus (l:name list) (l':name list) =
    let f (n:name) = not (List.mem n l')
    in List.filter f l
  in
  let f (ns:name list) ((k:action),(p:proc)) =
    (Action.free_names k) @ (minus (Process.free_names p) (Action.bound_names k)) @ ns
  in remove_duplicates (minus (List.fold_left f [] c.actions) (Value.channels c.restricted))

(* finds the nth occurrence of the given action *)
let find_action (index0:float) (k0:action) (c:t) = (* index >= 0 *)
  let rec f (index:float) (l:(action * proc) list) = match l with
    [] -> None
  | (k,p)::l -> (*Io.println (string_of_float index); *)
      let index:float = (if (Action.compare k0 k) = 0 then (index -. (Action.rate k)) else index) in
	if index<0.0
	then Some(index0-.index,c.restricted,k,p)
	else f index l
  in f index0 c.actions

let display (html:bool) (c:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let keyword (s:string) = if html then "<font color=#000075><b>" ^ s ^"</b></font>" else s in (*#ff6600 *)
  let newline = if html then "<br>" else "\n" in
  let space = if html then "&nbsp;" else " " in
  let rec indents (i:int) = if i<=0 then newline else indents (i-1) ^ space in
  let action_process (k:action) (p:proc) =
    if p = Process.Null then Action.display html k
    else Action.display html k ^ symbol "; " ^ Process.display html p
  in
  let s:string =
    if c.restricted = []
    then ""
    else indents 2 ^ keyword "new " ^ Value.displays html c.restricted
  in match c.actions with
      [] -> s ^ " () "
    | [k,p] -> (if s = "" then "" else s ^ indents 2 ^ keyword "run " ) ^ action_process k p
    | (k,p)::l ->
	let f (k,p) (acc:string) = indents 2 ^ symbol "or " ^ action_process k p ^ acc
	in s ^ indents 2 ^ keyword "do " ^ action_process k p ^ List.fold_right f l ""

let rec eval (c:t) =
  let f (k,p) = (Action.eval k,p)
  in {restricted = List.map Value.eval c.restricted; actions = List.map f c.actions}

let to_string (c:t) =
  let action_process (k:action) (p:proc) =
    if p = Process.Null then Action.to_string "" k
    else Action.to_string "" k ^ "; " ^ Process.to_string p
  in
  let s:string =
    if c.restricted = []
    then ""
    else "new " ^ Value.to_string (Value.Tuple c.restricted)
  in match c.actions with
      [] -> s ^ " () "
    | [k,p] -> (if s = "" then "" else s ^ "run " ) ^ action_process k p
    | (k,p)::l ->
	let f (k,p) (acc:string) = "or " ^ action_process k p ^ acc
	in s ^ "do " ^ action_process k p ^ List.fold_right f l ""
let to_html (c:t) = display true c

(*****************************************************************************)
