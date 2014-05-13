type position = Lexing.position
type typ = Typ.t
type name = string
type env = Env.t
type op =
    Plus
  | Minus
  | Mul
  | Div
  | Equal
  | Different
  | Lt
  | Gt
  | Ltequal
  | Gtequal

type fn =
    Float2Int
  | Int2Float
  | Sqrt

type t =
    String of string
  | Int of int
  | Channel of (string*int*t*t*typ)
  | Process of (string*int*typ)
  | Bool of bool
  | Char of char
  | Float of float
  | Data of name*t list
  | Cons of t*t
  | Name of name
  | Op of (t*op*t)
  | Neg of t
  | Show of t
  | Function of (fn*t)
  | Tuple of t list
  | List of t list
  | Coordinate of float*float*float
  | Space of Shape.t*t

type substitution = (name*t) list

let origin = Coordinate(0.0, 0.0, 0.0)

let empty = Tuple []

let init (n:name) = Name(n)

let compare (v:t) (v':t) = compare v v'

let display_function  (html:bool) (fn:fn) =
  let symbol (s:string) = if html then "<font color=#990000>" ^ s ^"</font>" else s
  in match fn with
    Float2Int -> symbol "int_of_float "
  | Int2Float -> symbol "float_of_int "
  | Sqrt ->  symbol "sqrt "

let display (html:bool) (v:t) =
  let symbol (s:string) = if html then "<font color=#990000>" ^ s ^"</font>" else s in
  let string (s:string) = if html then "<font color=#009900>\"" ^ s ^ "\"</font>" else "\"" ^ s ^ "\"" in
  let char (s:string) = if html then "<font color=#009900>'" ^ s ^ "'</font>" else s in
  let operator (o:op) =
    match o with
    | Plus -> symbol " + "
    | Minus -> symbol " - "
    | Mul -> symbol " * "
    | Div -> symbol " / "
    | Equal -> symbol " = "
    | Different -> symbol " <> "
    | Lt -> symbol " < "
    | Gt -> symbol " > "
    | Ltequal -> symbol " <= "
    | Gtequal -> symbol " >= "
  in
  let display_coords (x:float) (y:float) (z:float) =
    symbol "(" ^
    (string_of_float x) ^ symbol " ," ^
    (string_of_float y) ^ symbol " ," ^
    (string_of_float z) ^ symbol ")"
  in
  let rec display (v:t) =
    match v with
    | String(s) -> string s
    | Int(n) -> string_of_int n
    | Channel(s,i,r,rd,ty) -> s ^ (if i = 0 then (*symbol "@" ^ display r*) "" else "~" ^ string_of_int i)
    | Process(s,i,ty) -> s ^  (if i = 0 then "" else "~" ^ string_of_int i)
    | Bool(b) -> symbol (string_of_bool b)
    | Char(c) -> char (string_of_int (int_of_char c))
    | Float(f) -> string_of_float f
    | Data(n,vs) -> n ^ displays vs
    | Cons(v,v') -> display v ^ symbol "::" ^ display v'
    | Name(n) -> n
    | Op(v,o,v') -> symbol "(" ^ display v ^ operator o ^ display v' ^ symbol ")"
    | Neg(v) -> symbol "-" ^ display v
    | Show(v) -> symbol "show " ^ display v
    | Function(fn,v) -> symbol "(" ^ display_function html fn ^ display v ^ symbol ")"
    | Tuple(vs) -> displays vs
    | List([]) -> symbol "[]"
    | List(v::l) -> display v ^ symbol "::" ^ display (List l)
    | Space(s,v) -> Shape.display html s ^ "::" ^ display v
    | Coordinate(x,y,z) -> display_coords x y z
  and displays (vs:t list) =
    let rec f (vs:t list) =
      match vs with
      | [] -> ""
      | [v] -> display v
      | v::vs -> display v ^ symbol "," ^ f vs
  in symbol "(" ^ f vs ^ symbol ")"
    in display v

let displays (html:bool) (vs:t list) = display html (Tuple vs)
let to_string (v:t) = display false v
let to_strings (vs:t list) = displays false vs
let to_html (v:t) = display true v

let merge (v:t) (v':t) = match v,v' with
    Tuple(vs),Tuple(vs') -> Tuple(vs@vs')
  | v,Tuple(vs) -> Tuple(v::vs)
  | Tuple(vs),v -> Tuple(vs@[v])
  | _ -> Tuple [v;v']

let rate (v:t) = match v with
    Channel(s,i,Float(f),ty, rd) -> f
  | Process (_) -> infinity
  | Float(f) -> f
(*  | Channel(s,i,r,ty) -> Io.error ("Unevaluated rate " ^ to_string r ^ " in channel " ^ to_string v); 0.0 *)
  | _ -> (* Io.error ("No rate for " ^ to_string v) ; *) 0.0

let rec source (v:t) = match v with
    Tuple(vs) -> Tuple(List.map source vs)
  | Channel(s,i,r,rd,ty) -> Name(s)
  | Process(s,i,ty) -> Name(s)
  | _ -> v

let rec channels (vs:t list) = match vs with
  | (Channel(s,i,r,rd,ty)) :: vs -> s :: (channels vs)
  | _ -> []

let rec res (counter:int) (vs:t list) =
  match vs with (* assumes single occurence of a given name *)
  | (Channel(s,i,Float(f),Float(rd),ty)) :: vs  ->
      (s,Channel(s,counter,Float(f),Float(rd),ty)) :: res counter vs
  | (Channel(s,i,r,rd,ty))::vs -> failwith ("Unevaluated rate " ^ to_string r)
  | _ -> []

let rec free_names (v:t) =
  match v with
  | Data(n,v) -> List.flatten (List.map free_names v)
  | Cons(v,v') -> (free_names v) @ (free_names v')
  | Name(n) -> [n]
  | Op(v,op,v') -> (free_names v) @ (free_names v')
  | Neg(v) -> free_names v
  | Show(v) -> free_names v
  | Function(fn,v) -> free_names v
  | Tuple([]) -> []
  | Tuple(v::vs) -> (free_names v) @ (free_names (Tuple vs))
  | List([]) -> []
  | List(v::vs) -> (free_names v) @ (free_names (List vs))
  | Channel(s,i,r,rd,ty) -> (free_names r) @ (free_names rd)
  | _ -> []

let rec eval (v:t) = match v with
    Op(v,Plus,v') -> (
      match (eval v,eval v') with
	  Int(i),Int(i') -> Int(i + i')
	| Float(f),Float(f') -> Float(f +. f')
	| Bool(b),Bool(b') -> Bool(b || b')
	| String(s),String(s') -> String(s ^ s')
	| List(l),List(l') -> List(l @ l')
	| v,v' -> v
    )
  | Op(v,Minus,v') -> (
      match (eval v,eval v') with
	  Int(i),Int(i') -> Int(i - i')
	| Float(f),Float(f') -> Float(f -. f')
	| v,v' -> v
    )
  | Op(v,Mul,v') -> (
      match (eval v,eval v') with
	  Int(i),Int(i') -> Int(i * i')
	| Float(f),Float(f') -> Float(f *. f')
	| Bool(b),Bool(b') -> Bool(b && b')
	| v,v' -> v
    )
  | Op(v,Div,v') -> (
      match (eval v,eval v') with
	  Int(i),Int(i') -> Int(i / i')
	| Float(f),Float(f') -> Float(f /. f')
	| v,v' -> v
    )
  | Op(v,Equal,v') -> Bool(eval v = eval v')
  | Op(v,Different,v') -> Bool(eval v <> eval v')
  | Op(v,Lt,v') -> Bool(eval v < eval v')
  | Op(v,Gt,v') -> Bool(eval v > eval v')
  | Op(v,Ltequal,v') -> Bool(eval v <= eval v')
  | Op(v,Gtequal,v') -> Bool(eval v >= eval v')
  | Neg(v) -> (
      match eval v with
	  Bool(b) -> Bool(not b)
	| Int(i) -> Int(-i)
	| Float(i) -> Float(-.i)
	| v -> v
    )
  | Show(v) -> String(to_string (eval v))
  | Function(fn,v) -> (
      match fn, (eval v) with
	  Int2Float,Int(i) -> Float(float_of_int i)
	| Float2Int,Float(f) -> Int(int_of_float f)
	| Sqrt,Float(f) -> Float(sqrt f)
	| _,_ -> failwith ("incorrect evaluation of function " ^ display_function false fn)
    )
  | Tuple(l) -> Tuple(List.map eval l)
  | List(l) -> List(List.map eval l)
  | Cons(v,v') -> (
      match eval v,eval v' with
	  v,List(vs) -> List(v::vs)
	| v,v' -> Cons(v,v')
    )
  | Data(n,v) -> Data(n,List.map eval v)
  | Channel(s,i,r,rd,ty) -> Channel(s,i,eval r,eval rd,ty)
  | Space(s,p) -> Space(s, eval p)
  | v -> v

let rec bind (l:substitution) (v:t) =
  if l=[] then v
  else match v with
    Name(n) -> (
      try List.assoc n l
      with Not_found -> Name(n))
  | Data(n,v) -> Data(n,List.map (bind l) v)
  | Cons(v,v') -> Cons(bind l v,bind l v')
  | Op(v,op, v') -> Op(bind l v,op,bind l v')
  | Neg(v) -> Neg(bind l v)
  | Show(v) -> Show(bind l v)
  | Function(f,v) -> Function(f, bind l v)
  | Tuple(vs) -> Tuple(List.map (bind l) vs)
  | List(vs) -> List(List.map (bind l) vs)
  | Channel(s,i,r,rd,ty) -> Channel(s,i,bind l r,bind l rd,ty)
  | Space(s,v) -> Space(s, bind l v)
  | v -> v

let rec bindt (n:name) (t:typ) (v:t) = match v with
  | Tuple(vs) -> Tuple(List.map (bindt n t) vs)
  | List(vs) -> List(List.map (bindt n t) vs)
  | Channel(s,i,r,rd,t') -> Channel(s,i,r,rd,Typ.bind n t t')
  | v -> v

let rec case (v:t) (v':t) =
  match v,v' with
  | v,Name(n) -> Some [n,v]
  | Data(n,v),Data(n',v') -> if n=n' then case (Tuple v) (Tuple v') else None
  | Tuple([]),Tuple([]) -> Some []
  | Tuple(v::vs),Tuple(v'::vs') -> (
    match case v v' with
    | None -> None
    | Some bindings ->
        match case (Tuple vs) (Tuple vs') with
        | None -> None
        | Some bindings' -> Some(bindings@bindings'))
  | List([]),List([]) -> Some []
  | List(v::l),Cons(v',v'') -> (
    match case v v' with
    | None -> None
    | Some bindings -> match case (List l) v'' with
    | None -> None
    | Some bindings' -> Some(bindings@bindings'))
  | List(v::l),List(v'::l') -> (
    match case v v' with
    | None -> None
    | Some bindings -> match case (List l) (List l') with
    | None -> None
    | Some bindings' -> Some(bindings@bindings'))
  | v,v' -> if v=v' then Some [] else None

let rec typecheck (e:env) (v:t) =
  match v with
  | String(_) -> Typ.String
  | Int(_) -> Typ.Int
  | Channel(_,_,_,_,ty) -> ty
  | Process(_,_,ty) -> ty
  | Bool(b) -> Typ.Bool
  | Char(c) -> Typ.Char
  | Float(f) -> Typ.Float
  | Data(n,v) -> Typ.Data([n,List.map (typecheck e) v])
  | Cons(v,v') -> (
      match typecheck e v , typecheck e v' with
	  ty,Typ.List(ty') -> Typ.List(Typ.subtype ty ty')
	| ty,ty' -> failwith ( to_string v' ^ " of type " ^ Typ.to_string ty' ^ " is not a list")
    )
  | Name(n) -> (
      match Env.find n e with
	  Some(ty) -> ty
	| None -> failwith ( "Variable " ^ n ^ " is not global")
    )
  | Op(v,op,v') ->
      let ty:typ = typecheck e v in
      let ty':typ = typecheck e v' in
	if Typ.compatible ty ty'
	then match op with
	    Plus -> ty
	  | Minus -> ty
	  | Mul -> ty
	  | Div -> ty
	  | Equal -> Typ.Bool
	  | Different -> Typ.Bool
	  | Lt -> Typ.Bool
	  | Gt -> Typ.Bool
	  | Ltequal -> Typ.Bool
	  | Gtequal -> Typ.Bool
	else failwith ( to_string v ^ " of type " ^ Typ.to_string ty ^
	  " and " ^ to_string v' ^ " of type " ^ Typ.to_string ty' ^ " have incompatible types"
	)
  | Neg(v) ->  typecheck e v
  | Show(v) ->  Typ.String
  | Function(f,v) -> (
      match f with
	  Float2Int -> if typecheck e v = Typ.Float then Typ.Int else failwith (to_string v ^ "is not a float")
	| Int2Float -> if typecheck e v = Typ.Int then Typ.Float else failwith (to_string v ^ "is not an int")
	| Sqrt -> if typecheck e v = Typ.Float then Typ.Float else failwith (to_string v ^ "is not a float")
    )
  | Tuple(l) -> Typ.Tuple(List.map (typecheck e) l)
  | List([]) -> Typ.List(Typ.Poly "Any")
  | List(v::vs) -> Typ.List(typecheck e v)
  | Coordinate(x,y,z) -> Typ.Coordinate
  | Space(s,c) -> (
      let ty:typ = typecheck e c in
      if ty == Typ.Coordinate then Typ.Space else failwith(to_string c ^ "is not coordinate"))

let rec case_type (e:env) (ty:typ) (v:t) = match ty,v with
    ty,Name(n) -> [n,ty]
  | Typ.Data(l),Data(n,vs) -> (
      try case_type e (Typ.Tuple (List.assoc n l)) (Tuple vs)
      with Not_found -> failwith
	  ("Value " ^ to_string v ^ " does not match type " ^ Typ.to_string ty)
    )
  | Typ.Tuple([]),Tuple([]) -> []
  | Typ.Tuple(ty::tys),Tuple(v::vs) -> (case_type e ty v) @ (case_type e (Typ.Tuple tys) (Tuple vs))
  | Typ.List(ty),List([]) -> []
  | Typ.List(ty),Cons(v,v') -> (case_type e ty v) @ (case_type e (Typ.List ty) v')
  | Typ.List(ty),List(v::vs) -> (case_type e ty v)@ (case_type e (Typ.List ty) (List vs))
  | Typ.Recursive(n,ty),v -> case_type e (Typ.bind n (Typ.Recursive(n,ty)) ty) v
  | ty,v -> let _ = Typ.subtype (typecheck e v) ty in []
(*****************************************************************************)
