(*****************************************************************************)
type name = string
type value = Value.t
type pattern = Pattern.t
type species = Species.t
type typ = Typ.t
type substitution = (name*value) list
type t =
    Null
  | Parallel of t list
  | New of (value list * t)
  | Instance of (value*value list)
  | Match of (value*(value*t) list)
  | Value of (pattern*value*t)
  | Repeat of (int*t)

let display (html:bool) (p:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let keyword (s:string) = if html then "<font color=#000075><b>" ^ s ^"</b></font>" else s in (*#ff6600 *)
  let newline = if html then "<br>" else "\n" in
  let arrow = if html then symbol " -&gt; " else " -> " in
  let space = if html then "&nbsp;" else " " in
  let rec indents (i:int) = if i<=0 then newline else indents (i-1) ^ space in
  let rec display (p:t) (i:int) =
    let indent:string = indents i
    in match p with
        Null -> symbol "()"
      | Parallel([]) -> symbol "()"
      | Parallel([p]) -> symbol "(" ^ display p i ^ symbol ")"
      | Parallel(p::ps) ->
	  let f (p:t) (s:string) = indent ^ symbol "| " ^ display p (i+2) ^ s
	  in indent ^ symbol "( " ^ display p (i+2) ^ (List.fold_right f ps "") ^ indent ^ symbol ")"
      | New(vs,p) -> indent ^ keyword "new " ^ Value.displays html vs ^ display p i
      | Match(v,l) ->
	  let f (v,p) (acc:string) =
	    indent ^ keyword "case " ^ Value.display html v ^ arrow ^ indents (i+2) ^ display p (i+2) ^ acc
	  in indent ^ keyword "match " ^ Value.display html v ^ List.fold_right f l ""
      | Instance(v,vs) -> Value.display html v ^ Value.displays html vs
      | Value(m,v,p) -> indent ^ keyword "let " ^ Pattern.to_string m ^ " = " ^ Value.to_string v ^ indents (i+2) ^ display p (i+2)
      | Repeat(counter,p) -> string_of_int counter ^ symbol " of " ^ display p (i)
  in display p 2

let to_string (p:t) = display false p
let to_html (p:t) = display true p

let minus (l:name list) (l':name list) =
  let f (n:name) = not (List.mem n l')
  in List.filter f l

let rec free_names (p:t) = match p with
    Null -> []
  | Parallel(ps) ->
      let f (ns:name list) (p:t) = (free_names p) @ ns
      in List.fold_left f [] ps
  | New(vs,p) -> minus (free_names p) (Value.channels vs)
  | Match(v,l) ->
      let f (ns:name list) ((v:value),(p:t)) = (minus (free_names p) (Value.free_names v)) @ ns
      in  List.fold_left f (Value.free_names v) l
  | Instance(v,vs) -> (Value.free_names v) @ (List.flatten (List.map Value.free_names vs))
  | Value(m,v,p) -> (Value.free_names v) @ (minus (free_names p) (Pattern.free_names m))
  | Repeat(counter,p) -> free_names p

let rec unbind (l:name list) (b:substitution) = match l,b with
    l,[] -> []
  | [],b -> b
  | n::l,b -> unbind l (List.remove_assoc n b)

let rec bind (s:substitution) (p:t) = if s = [] then p else match p with
      Null -> Null
    | Parallel(l) -> Parallel(List.map (bind s) l)
    | New(vs,p) -> New(List.map (Value.bind s) vs,bind (unbind (Value.channels vs) s) p)
    | Instance(v,vs) -> Instance(Value.bind s v,List.map (Value.bind s) vs)
    | Match(v,l) ->
	let f (v,p) =
	  let bound_names:name list = Value.free_names v in
	  let s:substitution = unbind bound_names s
	  in v,bind s p
	in Match(Value.bind s v, List.map f l)
    | Value(m,v,p) -> Value(m, Value.bind s v,bind (unbind (Pattern.free_names m) s ) p)
    | Repeat(i,p) -> Repeat(i,bind s p)

let bind_pattern (vs:value list) (ms:pattern list) (p:t) = bind (List.flatten (List.map2 Pattern.bind_value vs ms)) p

let to_species (p:t) =
  let rec to_species (p:t) =
    match p with
        Null -> []
      | Parallel([]) -> []
      | Parallel(ps) ->
          (List.fold_right (fun p acc -> acc @ to_species p) ps [])
      | New(vs,p) -> failwith("New cannot b converted to species")
      | Match(v,l) -> failwith("Match cannot b converted to species")
      | Instance(v,vs) -> [(v,vs)]
      | Value(m,v,p) -> failwith("Value cannot b converted to species")
      | Repeat(1,p) -> to_species p
      | Repeat(counter,p) -> to_species (Repeat(counter-1,p)) @ to_species p
  in
  to_species p
(*****************************************************************************)
