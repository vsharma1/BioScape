(*****************************************************************************)
type name=string
type position = Lexing.position
exception Error of position * string
type t =
    Void
  | String
  | Int
  | Bool
  | Char
  | Float
  | Poly of name
  | Name of name
  | Channel of t list
  | Process of t list
  | Tuple of (t list)
  | List of t
  | Data of (name*t list) list
  | Recursive of (name*t)
  | Coordinate
  | Shape
  | Space

let display (html:bool) (t:t) =
  let symbol (s:string) = if html then "<font color=#990000>" ^ s ^"</font>" else s in
  let rec display (t:t) = match t with
      Void -> symbol "()"
    | String -> "string"
    | Int -> "int"
    | Bool -> "bool"
    | Char -> "char"
    | Float -> "float"
    | Poly(n) -> symbol "'" ^ n
    | Name(n) -> n
    | Process(ts) -> "proc" ^ displays ts
    | Channel([]) -> "chan"
    | Channel(ts) -> "chan" ^ displays ts
    | Tuple(ts) -> displays ts
    | List(t) -> "list" ^ symbol "(" ^ display t ^ symbol ")"
    | Data([]) -> ""
    | Data((n,ts)::[]) -> n ^ displays ts
    | Data((n,ts)::l) -> n ^ displays ts ^ " | " ^ display (Data l)
    | Recursive(n,t) -> n
    | Coordinate -> "coordinate"
    | Shape -> "shape"
    | Space -> "space"
  and displays (ts:t list) =
    let rec f (ts:t list) = match ts with
	[] -> ""
      | [m] -> display m
      | m::ts -> display m ^ symbol "," ^ f ts
    in symbol "(" ^ f ts ^ symbol ")"
  in display t

let displays (html:bool) (ts:t list) = display html (Tuple ts)
let to_string (t:t) = display false t
let to_strings (ts:t list) = displays false ts
let to_html (t:t) = display true t

let rec bind (n:name) (t:t) (t':t) = match t' with
    Poly(n) -> Poly(n)
  | Channel(ts) -> Channel(List.map (bind n t) ts)
  | Process(ts) -> Process(List.map (bind n t) ts)
  | Tuple(ts) -> Tuple(List.map (bind n t) ts)
  | List(t') -> List(bind n t t')
  | Data(ts) ->
      let f ((n':name),(ts:t list)) = (n',List.map (bind n t) ts)
      in Data(List.map f ts)
  | Recursive(n',t') ->
      if n=n'
      then Recursive(n',t')
      else Recursive(n',bind n t t')
  | Name(n') ->
      if n=n'
      then t
      else Name(n')
  | t' -> t'

let unfold (ty:t) = match ty with
  | Recursive(n,ty) -> bind n (Recursive(n,ty)) ty
  | _ -> ty

let subtype (t:t) (t':t) =
  let rec subtype (t:t) (t':t) (bindings:(t*t) list) =
    let add_subtype (t0:t) (t:t) (bindings:(t*t) list) =
      try
	let t':t = List.assoc t0 bindings
	in if t=t' then bindings else subtype t t' bindings (* N.B. compatible t t' bindings ? *)
      with Not_found -> (t0,t)::bindings
    in
      if List.mem (t',t) bindings (* inverted lookup *)
      then bindings
      else match t,t' with
      	t,Poly(n) -> add_subtype (Poly n) t bindings
      | Channel(ts),Channel(ts') -> subtypes ts ts' bindings
      | Process(ts),Process(ts') -> subtypes ts ts' bindings
      | Tuple(ts),Tuple(ts') -> subtypes ts ts' bindings
      | List(t),List(t') -> subtype t t' bindings
      | Data([]),Data(l') -> bindings
      | Data((n,ts)::l),Data(l') -> (
	  try
	    let ts':t list = List.assoc n l'
	    in subtype (Data l) (Data l') (subtypes ts ts' bindings)
	  with Not_found ->
	    failwith ("Type " ^ to_string (Data [n,ts]) ^ " is not a subtype of " ^ to_string t' ^ ". " )
	)
      | Recursive(n,t1),Recursive(n',t1') -> subtype (bind n t t1) (bind n' t' t1')  ((t',t)::bindings) (* unfold *)
      | t,Recursive(n',t1') -> subtype t (bind n' t' t1')  ((t',t)::bindings) (* unfold *)
      | Recursive(n,t1),t' ->  subtype (bind n t t1) t' ((t',t)::bindings) (* unfold *)
      | t,t' ->
	  if t = t'
	  then bindings
	  else failwith ("Type " ^ to_string t ^ " is not a subtype of " ^ to_string t' ^ "" )
  and subtypes (ts:t list) (ts':t list) (bindings:(t*t) list) = match ts,ts' with
    | [],[] -> bindings
    | t::ts,t'::ts' -> subtypes ts ts' (subtype t t' bindings)
    | _,_ -> failwith ("Type " ^ to_string (Tuple ts) ^ " is not a subtype of " ^ to_string (Tuple ts') ^ "" )
  in
  let _ = subtype t t' [] in t

let subtypes (ts:t list) (ts':t list) = subtype (Tuple ts) (Tuple ts')

let compatible (t:t) (t':t) =
  try let _ = subtype t t' in true
  with e ->
    try let _ = subtype t' t in true
    with e -> false

let rec instantiate (suffix:string) (t:t) = match t with
    Poly(n) -> Poly(n^suffix)
  | Channel(t) -> Channel(List.map (instantiate suffix) t)
  | Process(t) -> Process(List.map (instantiate suffix) t)
  | Tuple(ts) -> Tuple(List.map (instantiate suffix) ts)
  | List(t) -> List(instantiate suffix t)
  | Data(l) ->
      let rec f ((n:name),(ts:t list)) = (n,List.map (instantiate suffix) ts)
      in Data(List.map f l)
  | Recursive(n,t) -> Recursive(n,instantiate suffix t)
  | t -> t

let rec free_names (t:t) = match t with
    Poly(n) -> []
  | Channel(ts) -> List.flatten (List.map free_names ts)
  | Process(ts) -> List.flatten (List.map free_names ts)
  | Tuple(ts) -> List.flatten (List.map free_names ts)
  | List(t) -> free_names t
  | Data([]) -> []
  | Data((n,ts)::l) -> List.flatten (List.map free_names ts) @ (free_names (Data l))
  | Recursive(n,t) -> List.filter ((<>) n) (free_names t)
  | Name(n) -> [n]
  | t -> []

let channel (t:t) = match t with
  | Channel(ts) -> Some(ts)
  | _ -> None

(*****************************************************************************)
