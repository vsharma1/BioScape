(*****************************************************************************)
type name = string
type typ = Typ.t
type value = Value.t
type t =
    Wild
  | Name of name
  | Typed of name*typ
  | Tuple of t list
type substitution = (name*value) list

let init (n:name) = Name(n)
let compare (m:t) (m':t) = compare m m'

let display (html:bool) (m:t) =
  let symbol (s:string) = if html then "<font color=#990000>" ^ s ^"</font>" else s in
  let var (s:string) = if html then "<font color=#3333ff>" ^ s ^"</font>" else s in
  let rec display (m:t) = match m with
      Wild -> symbol "_"
    | Name(n) -> var n
    | Typed(n,ty) -> var n ^ symbol ":" ^ Typ.display html ty
    | Tuple(ms) -> displays ms
  and displays (ms:t list) =
    let rec f (ms:t list) = match ms with
	[] -> ""
      | [m] -> display m
      | m::ms -> display m ^ symbol "," ^ f ms
    in symbol "(" ^ f ms ^ symbol ")"
  in display m

let displays (html:bool) (ms:t list) = display html (Tuple ms)

let to_string (m:t) = display false m
let to_html (m:t) = display true m

let rec to_graph (m:t) =  match m with
    Wild -> "_"
  | Name(n) -> n
  | Typed(n,ty) -> n
  | Tuple(ms) -> to_graphs ms
and to_graphs (ms:t list) =
  let rec f (ms:t list) = match ms with
      [] -> ""
    | [m] -> to_graph m
    | m::ms -> to_graph m ^ "," ^ f ms
  in if ms=[] then "" else "(" ^ f ms ^ ")"

let rec free_names (m:t) = match m with
    Wild -> []
  | Name(n) -> [n]
  | Typed(n,ty) -> [n]
  | Tuple([]) -> []
  | Tuple(m::ms) -> (free_names m) @ (free_names (Tuple ms))

let rec bind (l:(name*name) list) (m:t) = match m with
    Wild -> m
  | Name(n) -> Name(try List.assoc n l with Not_found -> n)
  | Typed(n,ty) -> Typed((try List.assoc n l with Not_found -> n),ty)
  | Tuple(ms) -> Tuple(List.map (bind l) ms)

let rec bind_value (v:value) (m:t) = match v,m with
    v,Wild -> []
  | Value.Name(n'),Name(n) -> if n=n' then [] else [n,v]
  | v,Name(n) -> [n,v]
  | Value.Name(n'),Typed(n,ty) -> if n=n' then [] else [n,v]
  | v,Typed(n,ty) -> [n,v]
  | Value.Tuple [],Tuple [] -> []
  | Value.Tuple(v::vs),Tuple(m::ms) -> (bind_value v m)@(bind_value (Value.Tuple vs) (Tuple ms))
  | v,m -> []

let bind_values (vs:value list) (ms:t list) = bind_value (Value.Tuple vs) (Tuple ms)

let rec bindt (n:name) (t:typ) (m:t) = match m with
  | Typed(n',t') -> Typed(n',Typ.bind n t t')
  | Tuple(ms) -> Tuple(List.map (bindt n t) ms)
  | _ -> m

let rec bind_type (ty:typ) (m:t) = match ty,m with
    ty,Wild -> []
  | ty,Name(n) -> [n,ty]
  | ty,Typed(n,ty') -> [n,Typ.subtype ty ty']
  | Typ.Tuple [],Tuple [] -> []
  | Typ.Tuple(ty::tys),Tuple(m::ms) -> (bind_type ty m) @ (bind_type (Typ.Tuple tys) (Tuple ms))
  | ty,m -> failwith ("Pattern " ^ to_string m ^ " does not match type " ^ Typ.to_string ty)

let bind_types (ts:typ list) (ms:t list) = bind_type (Typ.Tuple ts) (Tuple ms) (*List.flatten (List.map2 bind_type ts ms)*)

let rec get_type (m:t) = match m with
    Wild -> failwith ("Pattern _ is untyped")
  | Name(n) -> failwith ("Pattern " ^ n ^ " is untyped")
  | Typed(n,ty) -> ty
  | Tuple(ms) -> Typ.Tuple(List.map get_type ms)

(*****************************************************************************)

