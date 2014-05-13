type token =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LT
  | GT
  | EOF
  | BANG
  | CARAT
  | BAR
  | EQUAL
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | LTGT
  | LTEQUAL
  | GTEQUAL
  | COMMA
  | DOT
  | AT
  | SEMI
  | COLON
  | UNDERSCORE
  | PRIME
  | NEW
  | IF
  | THEN
  | ELSE
  | STRING
  | INT
  | NULL
  | LET
  | TYPE
  | IN
  | OUT
  | SYSTEM
  | ARROW
  | REC
  | BOOL
  | CHAR
  | LIST
  | FLOAT
  | SUB
  | EMPTYLIST
  | TRUE
  | FALSE
  | CONS
  | SHOW
  | INT2FLOAT
  | FLOAT2INT
  | SQRT
  | TAU
  | AND
  | VAL
  | QUESTION
  | DO
  | REPLICATE
  | RUN
  | SAMPLE
  | MOV
  | DIRECTIVE
  | DELAY
  | OR
  | WITH
  | CHAN
  | PROC
  | PLOT
  | OF
  | AS
  | ALL
  | MATCH
  | CASE
  | SPACE
  | SHAPE
  | CUBOID
  | SPHERE
  | ORIGIN
  | COORD
  | INTVALUE of (int)
  | STRINGVALUE of (string)
  | NAME of (string)
  | FLOATVALUE of (float)
  | CHARVALUE of (char)

open Parsing;;
# 5 "parser.mly"
type position = Lexing.position
type name = string
type typ = Typ.t
type pattern = Pattern.t
type value = Value.t
type env = Env.t
type species = Species.t
type proc = Process.t
type action = Action.t
type choice = Choice.t
type definition = Definition.t
type shape = Shape.t
type space = Value.t
type environment = Environment.t
type volume = Volume.t
type substitution = (name*value) list
type directives = {
  duration:float;
  resolution:int;
  plots:(action*string) list * (species*string) list;
  graph:bool;
  debug:bool;
}

type t =
    Null
  | Parallel of t list
  | New of (position*value list*t)
  | Summation of ((position*action*t) list*bool)
  | Replicate of (position*action*t)
  | Instance of (position*value*value list)
  | Definitions of (position*name*pattern list*value*value*shape*t) list * t
  | Match of (value*(position*value*t) list)
  | Value of (position*pattern*value*t)
  | Type of (name*typ*t)
  | Repeat of (int*t)

(*IF-OCAML*)
module Store = Map.Make(String)
type store = (pattern list) Store.t
(*ENDIF-OCAML*)

(*F# let Store = Map.Make(String.compare)
type store = Tagged.Map<string,(pattern list)> F#*)

  let quote (id:string) =  "\"" ^ id ^ "\""

  let escaped (s:string) =
(*IF-OCAML*) String.escaped s (*ENDIF-OCAML*)
(*F#
   let f (c:char) = match c with
      | '"' -> "\\\""
      | '\n' -> "\\n"
      | _ -> String.of_char c
    in (String.map_concat f s)
F#*)

let substitution_to_string (s:substitution) =
  let rec list_to_string (ss:string list) = match ss with
        [] -> ""
      | [s] -> s
      | s::ss -> s ^ "," ^ list_to_string ss
  in
  let (ns:name list), (vs:value list) = List.split s in
    if s = [] then ""
    else "\n" ^ list_to_string ns ^ " := " ^ list_to_string (List.map Value.to_string vs)

let rec to_graph (env:store) (id:string) (p:t) =
  let edge (id:string) (id':string) (label:string) =
    "\n" ^ quote id ^ " -> " ^ quote id' ^ "[label = \" " ^ escaped label ^ " \"]"
  in
  let invisible_edge (id:string) (id':string) (label:string) =
    "\n" ^ quote id ^ " -> " ^ quote id' ^ "[color = white,arrowhead = none,label = \"" ^ escaped label ^ " \"]"
  in
  let box_node (id:string) = "\n" ^ quote id ^ "[shape=box]" in
  let solid_node (id:string) =
    (*"\n" ^ quote id ^ "[fillcolor=black,style=filled,label=\"\",height=.1]" *)
    "\n" ^ quote id ^ "[label=\"\",height=.15,width=.3]"
  in
  let fresh_node (id:string) (counter:int) = id ^ "." ^ string_of_int counter in
  let rec edge_to_graph (id:string) (id':string) (label:string) (p:t) = match p with
    | Instance(pos,v,vs) ->
        let n:string = Value.to_string v in
        let ms:pattern list =
          try Store.find n env (* NB: can fail if typechecking is not correct *)
          with Not_found -> []
        in
        let s:substitution = Pattern.bind_values vs ms
        in edge id (Value.to_string v) (label ^ substitution_to_string s)
    | Null -> edge id id' label ^ "\n" ^ quote id' ^ "[style = invis]"
    | Repeat(counter,p) -> edge_to_graph id id' (label ^ "\n[" ^ string_of_int counter ^ "]") p
    | Value(pos,m,v,p) ->
        let s:substitution = Pattern.bind_value v m
        in edge_to_graph id id' (label ^ substitution_to_string s ) p
    | Type(n,ty,p) ->
        let s:string = "type " ^ n ^ " := " ^ Typ.to_string ty
        in edge_to_graph id id' (label ^ "\n" ^ s ) p
    | _ -> solid_node id' ^ edge id id' label ^ to_graph env id' p
  in match p with
    | Null -> "\n" ^ quote id
    | Parallel(ps) ->
        let rec f (ps:t list) (acc:string) (counter:int) = match ps with
          | [] -> acc
          | p::ps -> edge_to_graph id (fresh_node id counter) "" p ^ f ps acc (counter+1)
        in box_node id ^ f ps "" 0
    | New(pos,vs,New(pos',vs',p)) -> to_graph env id (New(pos,vs@vs',p))
    | New(pos,vs,p) -> (if vs=[] then "" else invisible_edge id id (Value.to_strings vs ^ "\n ")) ^ to_graph env id p
    | Summation(l,move) ->
        let rec f (l:(position*action*t) list) (acc:string) (counter:int) = match l with
          | [] -> acc
          | (pos,k,p)::l -> edge_to_graph id (fresh_node id counter)
          (Action.to_string "" k) p ^ f l acc (counter+1)
        in (*oval_node id ^*) f l "" 0
    | Replicate(pos,k,p) ->
        "\n" ^ quote id ^ "[shape=ellipse,peripheries=2]" ^
        edge_to_graph id (fresh_node id 0) (Action.to_string "" k) p
    | Match(v,l) ->
        let rec f (l:(position*value*t) list) (acc:string) (counter:int) = match l with
          | [] -> acc
          | (pos,v',p)::l ->
              let label:string =
                if v' = Value.Bool(true) then Value.to_string v
                else if v' = Value.Bool(false) then "¬" ^ Value.to_string v
                else Value.to_string v ^ " = " ^ Value.to_string v'
              in edge_to_graph id (fresh_node id counter) label p ^ f l acc (counter+1)
        in (*oval_node id ^*) f l "" 0
    | Definitions(l,p) ->
        let f (env:store) ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) = Store.add n ms env in
        let env:store = List.fold_left f env l in
        let f (pos,n,ms,s,st,sh,p) (acc:string) =
          let label:string = n ^ Pattern.to_graphs ms
          in "\n" ^ quote n ^ "[label = \"" ^ label ^ "\"]" ^ to_graph env n p ^ acc
        in List.fold_right f l (to_graph env id p)
    | Instance(pos,v,vs) -> box_node id ^ edge_to_graph id (fresh_node id 0) "" p
(*box_node id ^ edge id (Value.to_string v) ""*)
    | Value(pos,m,v,p) ->
        let s:substitution = Pattern.bind_value v m in
        let label:string = substitution_to_string s
        in box_node id ^ edge_to_graph id (fresh_node id 0) label p
    | Type(n,ty,p) ->
        let label:string = "type " ^ n ^ " := " ^ Typ.to_string ty
        in box_node id ^ edge_to_graph id (fresh_node id 0) label p
    | Repeat(counter,p) ->
        let label:string = "[" ^ string_of_int counter ^ "]"
        in box_node id ^ edge_to_graph id (fresh_node id 0) label p

let rec global_graph (env:store) (p:t) = (*N.B. assumes that top-level variables are distinct! *)
  let counter = ref 0 in
  let fresh () = counter := (!counter + 1); string_of_int (!counter) in
  let repeat_edge (id:string) (counter:int) =
    "\n" ^ quote id ^ " -> " ^ quote id ^ "[color = white,arrowhead = none,label = \"[" ^ string_of_int counter ^ "]\\n \"]"
  in match p with
    | Null -> ""
    | Parallel(ps) ->
        let f (p:t) (acc:string) = global_graph env p ^ acc
        in List.fold_right f ps ""
    | New(pos,vs,p) -> global_graph env p (* N.B. assumes top-level restrictions are not repeated! *)
    | Definitions(l,p) -> (* copied from to_graph *)
        let f (env:store) ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) = Store.add n ms env in
        let env:store = List.fold_left f env l in
        let f (pos,n,ms,s,st,sh,p) (acc:string) =
          let label:string = n ^ Pattern.to_graphs ms
          in "\n" ^ quote n ^ "[label = \"" ^ label ^ "\"]" ^ to_graph env n p ^ acc
        in List.fold_right f l (global_graph env p)
    | Value(pos,m,v,p) -> global_graph env p
    | Type(n,ty,p) -> global_graph env p
    | Repeat(counter,Instance(pos,v,vs)) ->
        repeat_edge (Value.to_string v) counter ^ "\n" ^ global_graph env (Instance(pos,v,vs))
    | Repeat(counter,p) -> global_graph env p
    | Instance(pos,v,vs) -> "\n" ^ quote (Value.to_string v) ^ "[style=filled]"
    | _ -> to_graph Store.empty (fresh()) p

let init_graph (p:t) =
  let body:string = global_graph Store.empty p
  in "digraph G {node[fillcolor = yellow, fontsize = 20] edge[fontsize = 20,fontname=times] " ^ body ^ "}"

let rec bindt (n:name) (t:typ) (p:t) =
  let bind_action ((pos:position),(k:action),(p:t)) = pos,Action.bindt n t k, bindt n t p
  in match p with
      Null -> Null
    | Parallel(l) -> Parallel(List.map (bindt n t) l)
    | New(pos,vs,p) -> New(pos,List.map (Value.bindt n t) vs,bindt n t p)
    | Summation(l,move) -> Summation(List.map bind_action l, move)
    | Replicate(pos,k,p) -> Replicate(bind_action(pos,k,p))
    | Instance(pos,n,vs) -> Instance(pos,n,vs)
    | Definitions(l,p) ->
        let f ((pos:position),(n':name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
          pos,n',List.map (Pattern.bindt n t) ms,sp,st,sh,bindt n t p
        in Definitions(List.map f l, bindt n t p)
    | Match(v,l) ->
        let f ((pos:position),(v:value),(p:t)) = pos,v,bindt n t p
        in Match(v, List.map f l)
    | Value(pos,m,v,p) -> Value(pos,Pattern.bindt n t m,v,bindt n t p)
    | Type(n',ty,p) ->
        if n=n'
        then Type(n',ty,p)
        else Type(n',Typ.bind n t ty, bindt n t p)
    | Repeat(i,p) -> Repeat(i,bindt n t p)

let rec unbind (ns:name list) (s:substitution) = match ns,s with
    ns,[] -> []
  | [],s -> s
  | n::ns,s -> unbind ns (List.remove_assoc n s)

let rec bind (s:substitution) (p:t) =
  let bind_action ((pos:position),(k:action),(p:t)) =
    pos,Action.bind s k, bind (unbind (Action.bound_names k) s) p
  in match p with
      Null -> Null
    | Parallel(l) -> Parallel(List.map (bind s) l)
    | New(pos,vs,p) -> New(pos,List.map (Value.bind s) vs,bind (unbind (Value.channels vs) s) p)
    | Summation(l,move) -> Summation(List.map bind_action l, move)
    | Replicate(pos,k,p) -> Replicate(bind_action(pos,k,p))
    | Instance(pos,n,vs) -> Instance(pos,(Value.bind s n),List.map (Value.bind s) vs)
    | Definitions(l,p) ->
        let f ((pos:position),(n:name),(m:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
          pos,n,m,(Value.bind s sp),(Value.bind s st),sh,bind (unbind (List.flatten (List.map Pattern.free_names m)) s ) p
        in Definitions(List.map f l, bind s p)
    | Match(v,l) ->
        let f ((pos:position),(v:value),(p:t)) =
          let bound_names:name list = Value.free_names v in
          let s:substitution = unbind bound_names s
          in pos,v,bind s p
        in Match(Value.bind s v, List.map f l)
    | Value(pos,m,v,p) ->
        Value(pos,m, Value.bind s v,bind (unbind (Pattern.free_names m) s ) p)
    | Type(n,ty,p) -> Type(n,ty, bind s p)
    | Repeat(i,p) -> Repeat(i,bind s p)

let remove (ns:name list) (ns':name list) =
  let f (n:name) = not (List.mem n ns)
  in List.filter f ns'

let intersect (ns:name list) (ns':name list) =
  let f (n:name) = (List.mem n ns)
  in List.filter f ns'

let rec free_names (p:t) =
  let free_names_action ((pos:position),(k:action),(p:t)) =
    (Action.free_names k) @ (remove (Action.bound_names k) (free_names p))
  in match p with
  | Null -> []
  | Parallel(ps) -> List.flatten (List.map free_names ps)
  | New(pos,vs,p) -> remove (Value.channels vs) (free_names p)
  | Summation(l,move) -> List.flatten (List.map free_names_action l)
  | Replicate(pos,k,p) -> free_names_action (pos,k,p)
  | Instance(pos,n,vs) -> List.flatten (List.map Value.free_names vs)
  | Definitions(l,p) ->
      let f ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
        remove (List.flatten (List.map Pattern.free_names ms)) (free_names p)
      in (List.flatten (List.map f l)) @ (free_names p)
  | Match(v,l) ->
      let f ((pos:position),(v:value),(p:t)) =
        remove (Value.free_names v) (free_names p)
      in List.flatten (List.map f l)
  | Value(pos,m,v,p) -> remove (Pattern.free_names m) (free_names p)
  | Type(n,ty,p) -> free_names p
  | Repeat(i,p) -> free_names p

let bound_names (p:t) =
  let rec global (p:t) = match p with
    | Parallel(ps) -> List.flatten (List.map global ps)
    | New(pos,vs,p) -> global p
    | Definitions(l,p) -> global p
    | Value(pos,m,v,p) -> global p
    | Type(n,ty,p) -> global p
    | _ -> bound_names p (* Cannot have global repeat of a restriction *)
  and bound_names_action ((pos:position),(k:action),(p:t)) =
    (Action.bound_names k) @ (remove (Action.bound_names k) (bound_names p))
  and bound_names (p:t) = match p with
    | Null -> []
    | Parallel(ps) -> List.flatten (List.map bound_names ps)
    | New(pos,vs,p) -> (Value.channels vs)@(bound_names p)
    | Summation(l,move) -> List.flatten (List.map bound_names_action l)
    | Replicate(pos,k,p) -> bound_names_action (pos,k,p)
    | Instance(pos,n,vs) -> []
    | Definitions(l,p) ->
        let f ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
          (List.flatten (List.map Pattern.free_names ms))@(bound_names p)
        in (List.flatten (List.map f l)) @ (bound_names p)
    | Match(v,l) ->
      let f ((pos:position),(v:value),(p:t)) =
        (Value.free_names v)@(bound_names p)
      in List.flatten (List.map f l)
    | Value(pos,m,v,p) -> (Pattern.free_names m)@(bound_names p)
    | Type(n,ty,p) -> bound_names p
    | Repeat(i,p) -> bound_names p
  in global p

let global (p:t) =
  let counter = ref 0 in
  let fresh () = let i:int = !counter in counter := (i + 1); i in
  let rec global (p:t) = match p with
  | Parallel(l) -> Parallel(List.map global l)
  | New(pos,vs,New(pos',vs',p)) -> global (New(pos,vs@vs',p))
  | New(pos,vs,p) -> global (bind (Value.res (fresh()) (List.map Value.eval vs)) p)
  | Definitions(l,p) -> Definitions(l,global p)
  | Value(pos,m,v,p) -> global (bind (Pattern.bind_value v m) p)
  | Type(n,ty,p) -> global p
  | _ -> p (* Cannot have global repeat of a restriction *)
  in global p

let rec global2 (p:t) =
  match p with
  | Value(pos,m,v,p) -> global2 p
  | New(pos,vs,p) -> global2 p
  | _ -> p (* Cannot have global repeat of a restriction *)

let rec merge (p:t) =
  match p with
  | New(pos,vs,New(pos',vs',p)) -> merge (New(pos,vs@vs',p))
  | _ -> p

let encode (p:t) =
  let e:environment ref = ref Environment.empty in
  let vol:volume ref = ref Volume.empty in
  let counter = ref 0 in
  let fresh () = counter := (!counter + 1); string_of_int (!counter) in
  let add_definition (n:name) (m:pattern list) (sp:space) (st:value) (sh:shape) (d:definition) =
    let v:value = Value.Process(n,0,Typ.Void) in
    vol := Volume.add_spatialinfo (v, []) sp sh (Value.rate st) !vol;
    e := (Environment.add v m d !e) in
  let rec encode_actions (l:(position*action*t) list) =
    let f ((pos:position),(k:action),(p:t)) = k,encode p
    in List.map f l
  and create_instance (c:choice) =
    let n:value = Value.Process(fresh(),0,Typ.Void) in (*Value.Name(fresh())*)
    let ns:name list = Choice.free_names c in
    e := (Environment.add n (List.map Pattern.init ns) (Definition.Choice(c)) !e);
    Process.Instance(n,List.map Value.init ns)
  and encode (p:t) =
    match p with (* merge restrictions, create new definitions for summations *)
    | Null -> Process.Null
    | Parallel(l) -> Process.Parallel(List.map encode l)
    | New(pos,vs,New(pos',vs',p)) -> encode (New(pos,vs@vs',p))
    | New(pos,vs,Summation(l,move)) ->
        let c:choice = Choice.init vs (encode_actions l) in
        if Choice.ok c
        then create_instance c
        else raise (Typ.Error(pos,"Attempt to communicate on restricted channels " ^ Value.to_strings vs))
    | New(pos,vs,p) -> Process.New(vs,encode p)
    | Summation(l,move) -> create_instance (Choice.init [] (encode_actions l))
    | Instance(pos,n,v) -> Process.Instance(n,v)
    | Replicate(pos,k,p) ->
        let n:value = Value.Process(fresh(),0,Typ.Void) in
        let p:proc = encode p in
        let ns:name list = (Action.free_names k) @ (Process.free_names p) in
        let vs:value list = List.map Value.init ns in
        let c:choice = Choice.init [] [k,Process.Parallel [p;Process.Instance(n,vs)]]
        in e := (Environment.add n (List.map Pattern.init ns) (Definition.Choice(c))  !e); Process.Instance(n,vs)
    | Definitions(l,p) ->
        let f ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
          n,Value.Process(n,0,Typ.Void) in
        let s:substitution = List.map f l in
        let p:t = bind s p in
        let f (p':proc) ((pos:position),(n:name),(m:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
          let p:t = bind s p
          in match merge p with
          | New(pos,vs,Summation(l,move)) ->
              let st = if move then st else Value.Float 0. in
              let c:choice = Choice.init vs (encode_actions l) in
              if Choice.ok c
              then (add_definition n m sp st sh (Definition.Choice c); p')
              else raise (Typ.Error(pos,"Attempt to communicate on restricted channels " ^ Value.to_strings vs))
          | Summation(l,move) ->
              let st = if move then st else Value.Float 0. in
              add_definition n m sp st sh (Definition.Choice (Choice.init [] (encode_actions l))); p'
          | Null -> add_definition n m sp (Value.Float 0.) sh (Definition.Choice (Choice.init [] [])); p'
          | _ -> add_definition n m sp (Value.Float 0.) sh (Definition.Process (encode p)); p'
              in List.fold_left f (encode p) l
          | Match(v,l) ->
              let f ((pos:position),(v:value),(p:t)) = v, encode p
              in Process.Match(v, List.map f l)
          | Repeat(i,p) -> Process.Repeat(i,encode p)
          | Value(pos,m,v,p) -> Process.Value(m,v,encode p)
          | Type(n,ty,p) -> encode p in
  let p':proc = encode (global p)
  in (!e),(!vol),p',init_graph p

let display (html:bool) (p:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let keyword (s:string) = if html then "<font color=#000075><b>" ^ s ^"</b></font>" else s in (*#ff6600 *)
  let newline = if html then "<br>" else "\n" in
  let arrow = if html then symbol " -&gt; " else " -> " in
  let space = if html then "&nbsp;" else " " in
  let rec indents (i:int) = if i<=0 then newline else indents (i-1) ^ space in
  let rec display (p:t) (i:int) =
    let indent:string = indents i in
    let action_process (k:action) (p:t) (i:int) =
      if p = Null then Action.display html k
      else Action.display html k ^ symbol ";" ^ indents i ^ display p i
    in
    let rec parallel (l:t list) = match l with
        [] -> ""
      | Parallel(l')::l -> parallel (l'@l)
      | [p] -> display p (i+2)
      | p::l -> display p (i+2) ^ indent ^ symbol "| " ^ (parallel l)
    in match p with
        Null -> symbol "()"
      | Parallel(l) -> symbol "( " ^ parallel l ^ indent ^ symbol ")"
      | New(pos,vs,p) -> keyword "new " ^ Value.displays html vs ^ indent ^ display p i
      | Summation([],move) -> ""
      | Summation([pos,k,p],move) -> action_process k p i
      | Summation((pos,k,p)::l,move) ->
          let f (pos,k,p) (acc:string) = indent ^ symbol "or " ^ action_process k p (i+2) ^ acc
          in keyword "do " ^ action_process k p (i+2) ^ List.fold_right f l ""
      | Replicate(pos,k,p) -> symbol "*" ^ action_process k p (i+2)
      | Match(v,l) ->
          let f (pos,v,p) (acc:string) =
            indent ^ keyword "case " ^ Value.display html v ^ arrow ^ indents (i+2) ^ display p (i+2) ^ acc
          in keyword "match " ^ Value.display html v ^ List.fold_right f l ""
      | Definitions(l,p) ->
          let f (s:string) (pos,n,m,sp,st,sh,p) =
            indent ^ keyword "let " ^ n ^ Pattern.displays html m ^ " = " ^ display p (i+2) ^ s
          in List.fold_left f (indent ^ display p i) l
      | Instance(pos,v,vs) -> Value.display html v ^ Value.displays html vs
      | Value(pos,m,v,p) -> keyword "val " ^ Pattern.display html m ^ " = " ^ Value.display html v ^ indents (i+2) ^ display p (i+2)
      | Type(n,ty,p) -> keyword "type " ^ n ^ " = " ^ Typ.display html ty ^ indents (i+2) ^ display  p (i+2)
      | Repeat(counter,p) -> string_of_int counter ^ symbol " of " ^ display p (i)
  in display p 2

let to_string (p:t) = display false p
let to_html (p:t) = display true p

let unique_string (pos:position) = "~" ^ string_of_int pos.Lexing.pos_cnum

let bind_pattern (pos:position) (t:typ) (m:pattern) (e:env) =
  let suffix:string = unique_string pos in
  let t:typ = Typ.instantiate suffix t in
  let l:(name*typ) list = Pattern.bind_type t m
  in Env.add_list l e

let rec typecheck (e:env) (p:t) =
  let check_action ((pos:position),(k:action),(p:t)) =
    try
      let e:env = Action.typecheck (unique_string pos) e k
      in pos,k,typecheck e p
    with Failure(s) -> raise (Typ.Error(pos,"Invalid action " ^ Action.to_string
    "" k ^ ". " ^ s))
  in match p with
    | Null -> Null
    | Parallel(l) -> Parallel(List.map (typecheck e) l)
    | New(pos,vs,p) -> ( (* Fix *)
        try
          let f (v:value) (e:env) =
            let t:typ = Value.typecheck e v
            in match v with
              | Value.Channel(n,_,_,_,_) -> Env.add n (t) e
              | _ -> raise (Typ.Error(pos,Value.to_string v ^ " is not a channel."))
          in New(pos,vs,typecheck (List.fold_right f vs e) p)
        with Failure(s) -> raise (Typ.Error(pos,s))
      )
    | Summation(l,move) -> Summation(List.map check_action l,move)
    | Replicate(pos,k,p) -> Replicate(check_action(pos,k,p))
    | Instance(pos,v,vs) -> (
        try
          match Value.typecheck e v with
            | Typ.Process(ts) ->
                let sent:typ list = List.map (Value.typecheck e) vs in
                ignore(try Typ.subtypes sent ts
                with Failure(s) -> raise (
                  Typ.Error(pos,"Process " ^ Value.to_string v ^
                  " expects types " ^ Typ.to_strings ts ^
                  " but is given types " ^ Typ.to_strings sent ^ ". ")));
                Instance(pos,v,vs)
            | t -> raise (Typ.Error(pos,Value.to_string v ^ " of type " ^ Typ.to_string t ^ " is not a process."))
        with Failure(s) -> raise (Typ.Error(pos,s))
      )
    | Definitions(l,p) ->
        let f (e:env) ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) =
     let tsp:typ = Value.typecheck e sp in
     if tsp != Typ.Space then raise (Typ.Error(pos, Value.to_string sp ^ " of type " ^ Typ.to_string tsp ^ " is not a space."));
        (*
      match Env.find sp e with
      | None -> raise(Typ.Error(pos, "Space: '" ^ sp ^ "' not defined"))
      | Some(t) -> if t != Typ.Space then raise(Typ.Error(pos, "'" ^ sp ^ "' is not defined as space")) else ();
         *)
        try
          let ts:typ list = List.map Pattern.get_type ms
          in Env.add n (Typ.Process ts) e
          with Failure(s) -> raise (Typ.Error(pos,s))
        in
        let e:env = List.fold_left f e l in
    let f ((pos:position),(n:name),(ms:pattern list),(sp:value),(st:value),(sh:shape),(p:t)) = try
          let ts:typ list = List.map Pattern.get_type ms in
          let ts:typ list = List.map (Typ.instantiate (unique_string pos)) ts in
          let bindings:(name*typ) list = Pattern.bind_types ts ms
          in pos,n,ms,sp,st,sh,typecheck (Env.add_list bindings e) p
          with Failure(s) -> raise (Typ.Error(pos,s))
        in Definitions(List.map f l, typecheck e p)
    | Match(v,l) ->
        let t:typ = Value.typecheck e v in
        let f (pos,v,p) =
          try
            let bindings:(name*typ) list = Value.case_type e t v in
            let e:env = Env.add_list bindings e
            in pos,v,typecheck e p
          with Failure(s) -> raise (Typ.Error(pos,s))
        in Match(v, List.map f l)
    | Value(pos,m,v,p) -> (
        try
          let t:typ = Value.typecheck e v in
          let e:env = bind_pattern pos t m e
          in Value(pos,m,v,typecheck e p)
        with Failure(s) -> raise (Typ.Error(pos,s))
      )
    | Type(n,t,p) ->
        let t:typ =
          if List.mem n (Typ.free_names t)
          then Typ.Recursive(n,t)
          else t
        in Type(n,t,typecheck e (bindt n t p))
    | Repeat(i,p) -> Repeat(i,typecheck e p)

let check (p:t) =
  let initial_env =
    Env.add_list
      ["print",Typ.Process [Typ.String];
       "println",Typ.Process [Typ.String];
       "break",Typ.Process []]
      Env.empty
  in typecheck initial_env p

let pos = Parsing.symbol_start_pos
(* pos() should not be called inside a lambda expression  *)

let check_duplicates (ms:pattern list) (pos:position) =
  let rec check (l:name list) = match l with
      [] -> []
    | n::l ->
        if List.mem n l
        then raise (Typ.Error(pos,Pattern.displays false ms ^ "contains multiple occurrence of " ^ n))
        else check l
  in
  ignore(check (List.flatten (List.map Pattern.free_names ms)));
  ms

let initial_directives = {
  duration = infinity;
  resolution = 0;
  plots = [],[];
  graph=false;
  debug=false;
}

let export (d:directives) = d.duration,d.resolution,d.graph,d.debug,d.plots

(*****************************************************************************)

# 639 "parser.ml"
let yytransl_const = [|
  257 (* LPAREN *);
  258 (* RPAREN *);
  259 (* LBRACKET *);
  260 (* RBRACKET *);
  261 (* LT *);
  262 (* GT *);
    0 (* EOF *);
  263 (* BANG *);
  264 (* CARAT *);
  265 (* BAR *);
  266 (* EQUAL *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* STAR *);
  270 (* SLASH *);
  271 (* LTGT *);
  272 (* LTEQUAL *);
  273 (* GTEQUAL *);
  274 (* COMMA *);
  275 (* DOT *);
  276 (* AT *);
  277 (* SEMI *);
  278 (* COLON *);
  279 (* UNDERSCORE *);
  280 (* PRIME *);
  281 (* NEW *);
  282 (* IF *);
  283 (* THEN *);
  284 (* ELSE *);
  285 (* STRING *);
  286 (* INT *);
  287 (* NULL *);
  288 (* LET *);
  289 (* TYPE *);
  290 (* IN *);
  291 (* OUT *);
  292 (* SYSTEM *);
  293 (* ARROW *);
  294 (* REC *);
  295 (* BOOL *);
  296 (* CHAR *);
  297 (* LIST *);
  298 (* FLOAT *);
  299 (* SUB *);
  300 (* EMPTYLIST *);
  301 (* TRUE *);
  302 (* FALSE *);
  303 (* CONS *);
  304 (* SHOW *);
  305 (* INT2FLOAT *);
  306 (* FLOAT2INT *);
  307 (* SQRT *);
  308 (* TAU *);
  309 (* AND *);
  310 (* VAL *);
  311 (* QUESTION *);
  312 (* DO *);
  313 (* REPLICATE *);
  314 (* RUN *);
  315 (* SAMPLE *);
  316 (* MOV *);
  317 (* DIRECTIVE *);
  318 (* DELAY *);
  319 (* OR *);
  320 (* WITH *);
  321 (* CHAN *);
  322 (* PROC *);
  323 (* PLOT *);
  324 (* OF *);
  325 (* AS *);
  326 (* ALL *);
  327 (* MATCH *);
  328 (* CASE *);
  329 (* SPACE *);
  330 (* SHAPE *);
  331 (* CUBOID *);
  332 (* SPHERE *);
  333 (* ORIGIN *);
  334 (* COORD *);
    0|]

let yytransl_block = [|
  335 (* INTVALUE *);
  336 (* STRINGVALUE *);
  337 (* NAME *);
  338 (* FLOATVALUE *);
  339 (* CHARVALUE *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\001\000\001\000\016\000\016\000\016\000\016\000\
\016\000\003\000\003\000\003\000\003\000\003\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\017\000\017\000\019\000\019\000\
\014\000\014\000\020\000\020\000\013\000\013\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\006\000\006\000\006\000\006\000\022\000\022\000\007\000\007\000\
\007\000\008\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\023\000\
\023\000\005\000\005\000\005\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\025\000\025\000\026\000\026\000\
\010\000\010\000\010\000\024\000\021\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\001\000\001\000\002\000\003\000\002\000\
\003\000\001\000\003\000\004\000\002\000\003\000\007\000\009\000\
\005\000\005\000\005\000\003\000\003\000\002\000\001\000\001\000\
\007\000\005\000\005\000\005\000\003\000\002\000\001\000\001\000\
\001\000\003\000\002\000\004\000\002\000\002\000\007\000\004\000\
\006\000\004\000\003\000\003\000\010\000\003\000\003\000\003\000\
\001\000\003\000\003\000\005\000\001\000\003\000\005\000\003\000\
\004\000\002\000\005\000\003\000\004\000\002\000\003\000\002\000\
\001\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\001\000\003\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\001\000\003\000\
\003\000\003\000\003\000\001\000\002\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\004\000\001\000\
\003\000\001\000\001\000\001\000\002\000\003\000\003\000\003\000\
\003\000\003\000\001\000\007\000\004\000\008\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\074\000\135\000\000\000\023\000\
\004\000\033\000\000\000\000\000\136\000\002\000\000\000\000\000\
\000\000\000\000\000\000\000\000\094\000\077\000\078\000\000\000\
\000\000\000\000\000\000\000\000\000\000\076\000\075\000\000\000\
\080\000\079\000\000\000\000\000\103\000\000\000\108\000\139\000\
\000\000\065\000\000\000\140\000\000\000\073\000\141\000\142\000\
\000\000\000\000\110\000\111\000\114\000\112\000\000\000\113\000\
\000\000\000\000\123\000\122\000\124\000\000\000\143\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\049\000\000\000\037\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\096\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\109\000\000\000\000\000\131\000\117\000\118\000\125\000\
\000\000\000\000\000\000\000\000\000\000\000\000\044\000\034\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\000\000\
\000\000\000\000\003\000\000\000\000\000\043\000\000\000\054\000\
\001\000\000\000\000\000\011\000\101\000\000\000\100\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\085\000\000\000\000\000\000\000\000\000\000\000\107\000\106\000\
\000\000\068\000\066\000\072\000\071\000\000\000\121\000\000\000\
\000\000\000\000\000\000\126\000\000\000\000\000\000\000\000\000\
\031\000\029\000\000\000\048\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\050\000\
\000\000\007\000\009\000\000\000\040\000\036\000\012\000\000\000\
\105\000\000\000\000\000\133\000\000\000\070\000\000\000\128\000\
\119\000\130\000\129\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\017\000\000\000\000\000\018\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\027\000\028\000\
\000\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\039\000\000\000\132\000\000\000\
\025\000\000\000\000\000\052\000\134\000\016\000\000\000\000\000\
\045\000"

let yydgoto = "\010\000\
\030\000\037\000\039\000\252\000\064\000\068\000\071\000\031\000\
\087\000\160\000\032\000\033\000\034\000\106\000\035\000\112\000\
\101\000\094\000\095\000\253\000\060\000\148\000\123\000\061\000\
\088\000\153\000"

let yysindex = "\103\006\
\023\010\081\010\008\255\042\011\049\255\013\255\086\255\196\254\
\155\255\000\000\042\255\000\000\196\254\042\011\219\254\042\011\
\000\000\233\254\251\254\013\255\196\254\009\255\009\255\146\011\
\231\254\063\255\042\011\037\255\000\000\000\000\049\255\000\000\
\000\000\000\000\109\255\231\254\000\000\000\000\113\255\049\255\
\043\255\042\011\072\255\042\011\000\000\000\000\000\000\042\011\
\042\011\042\011\042\011\154\255\157\255\000\000\000\000\049\255\
\000\000\000\000\215\004\141\255\000\000\042\011\000\000\000\000\
\013\255\000\000\140\255\000\000\013\255\000\000\000\000\000\000\
\155\255\084\255\000\000\000\000\000\000\000\000\167\255\000\000\
\091\255\091\255\000\000\000\000\000\000\091\255\000\000\160\255\
\089\255\092\255\013\255\146\011\171\255\181\255\184\255\026\255\
\215\004\015\255\190\003\086\255\124\010\178\255\179\255\118\255\
\000\000\069\255\000\000\004\011\105\255\008\255\000\000\165\010\
\042\011\115\000\146\011\169\255\146\011\165\010\008\255\122\255\
\112\255\196\000\191\255\180\255\070\004\027\255\027\255\027\255\
\027\255\117\255\123\255\000\000\042\011\042\011\042\011\042\011\
\042\011\042\011\042\011\042\011\042\011\042\011\042\011\042\011\
\021\001\198\255\186\255\200\255\155\255\075\255\204\255\192\255\
\207\255\000\000\155\255\155\255\000\000\000\000\000\000\000\000\
\130\255\106\255\202\255\203\255\042\255\146\011\000\000\000\000\
\042\011\205\255\042\011\155\255\146\011\195\255\233\254\000\000\
\155\255\042\011\042\011\209\255\009\255\156\255\000\000\138\255\
\113\255\231\254\000\000\215\004\042\011\000\000\146\011\000\000\
\000\000\113\255\139\255\000\000\000\000\042\011\000\000\142\255\
\208\255\221\255\032\255\027\255\143\004\183\001\070\004\161\255\
\000\000\020\005\055\255\019\255\027\255\215\004\000\000\000\000\
\013\255\000\000\000\000\000\000\000\000\155\255\000\000\223\255\
\093\255\228\255\091\255\000\000\042\011\155\255\155\255\042\011\
\000\000\000\000\171\255\000\000\215\004\042\011\098\001\004\011\
\210\255\042\011\000\000\004\011\007\005\215\004\042\011\000\000\
\212\255\000\000\000\000\004\002\000\000\000\000\000\000\077\002\
\000\000\216\255\158\255\000\000\186\255\000\000\192\255\000\000\
\000\000\000\000\000\000\150\002\042\255\042\255\126\011\215\004\
\042\011\155\255\000\000\146\011\223\002\000\000\000\000\215\004\
\196\254\146\011\166\255\219\255\155\255\000\000\000\000\000\000\
\040\003\004\011\000\000\042\011\218\255\182\255\233\255\170\255\
\042\255\155\255\000\000\113\003\000\000\042\011\000\000\241\255\
\000\000\004\011\077\255\000\000\000\000\000\000\243\255\146\011\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\226\007\000\000\000\000\000\000\251\000\000\000\
\054\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\255\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\154\009\000\000\000\000\000\000\000\000\195\009\000\000\011\009\
\000\000\000\000\000\000\000\000\254\255\000\000\000\000\102\007\
\070\005\000\000\000\000\000\000\000\000\000\000\000\000\168\007\
\000\000\209\008\000\000\001\001\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\008\000\000\000\000\000\000\097\009\
\000\000\000\000\000\000\000\000\223\003\082\000\163\000\244\000\
\069\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\000\000\
\000\000\136\005\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\202\005\000\000\000\000\000\000\045\011\
\217\010\000\000\000\000\028\006\000\000\000\000\000\000\000\000\
\000\000\236\009\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\002\150\001\084\003\113\004\040\004\157\003\
\000\000\011\003\194\002\121\002\231\001\186\004\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\023\000\000\000\094\006\000\000\000\000\000\000\
\102\008\000\000\000\000\000\000\000\000\160\006\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\033\000\000\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\226\006\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\007\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\168\008\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\177\255\003\000\248\255\246\255\025\000\005\000\
\185\255\009\000\246\004\002\000\239\255\000\000\008\000\220\255\
\083\000\094\255\094\000\215\255\247\255\196\255\206\255\000\000\
\136\000\120\255"

let yytablesize = 3299
let yytable = "\118\000\
\081\000\152\000\234\000\038\000\105\000\107\000\059\000\040\000\
\151\000\103\000\041\000\146\000\072\000\065\000\013\000\013\000\
\097\000\096\000\099\000\226\000\029\000\067\000\116\000\133\000\
\134\000\104\000\062\000\014\000\014\000\114\000\185\000\120\000\
\139\000\109\000\171\000\066\000\172\000\134\000\169\000\194\000\
\139\000\110\000\011\000\098\000\122\000\139\000\125\000\132\000\
\013\000\062\000\126\000\127\000\128\000\129\000\147\000\111\000\
\063\000\100\000\150\000\133\000\134\000\014\000\021\000\021\000\
\145\000\143\000\089\000\016\000\139\000\026\000\026\000\142\000\
\017\000\143\000\090\000\102\000\220\000\219\000\143\000\063\000\
\164\000\093\000\113\000\224\000\225\000\008\001\069\000\170\000\
\029\000\158\000\159\000\156\000\217\000\067\000\010\001\091\000\
\021\000\022\000\023\000\092\000\240\000\143\000\176\000\026\000\
\115\000\244\000\030\001\031\001\032\001\183\000\222\000\121\000\
\027\000\187\000\040\000\188\000\070\000\041\000\069\000\193\000\
\028\000\157\000\029\000\040\000\174\000\229\000\041\000\230\000\
\180\000\117\000\179\000\181\000\182\000\119\000\049\001\203\000\
\204\000\205\000\206\000\207\000\208\000\209\000\210\000\211\000\
\212\000\213\000\214\000\001\001\070\000\251\000\007\001\052\000\
\053\000\124\000\130\000\073\000\006\001\131\000\013\001\014\001\
\144\000\149\000\097\000\248\000\154\000\133\000\134\000\155\000\
\161\000\162\000\135\000\237\000\163\000\239\000\139\000\140\000\
\141\000\142\000\074\000\166\000\245\000\246\000\167\000\075\000\
\076\000\168\000\184\000\177\000\178\000\191\000\195\000\196\000\
\199\000\077\000\078\000\079\000\080\000\200\000\201\000\216\000\
\000\001\218\000\034\001\217\000\202\000\221\000\005\001\143\000\
\223\000\222\000\227\000\231\000\232\000\041\001\242\000\249\000\
\250\000\238\000\255\000\081\000\082\000\247\000\004\001\002\001\
\009\001\003\001\050\001\083\000\084\000\011\001\025\001\012\001\
\085\000\027\001\015\001\086\000\040\001\020\001\047\001\028\001\
\016\001\019\001\053\001\098\000\021\001\022\001\023\001\039\001\
\045\001\024\001\137\000\048\001\056\001\046\001\138\000\031\000\
\022\000\243\000\081\000\236\000\052\001\081\000\081\000\081\000\
\030\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
\081\000\081\000\081\000\033\001\081\000\081\000\081\000\067\000\
\047\000\081\000\081\000\081\000\081\000\037\001\104\000\067\000\
\081\000\081\000\069\000\043\001\127\000\081\000\044\001\067\000\
\228\000\055\001\000\000\000\000\000\000\000\000\000\000\081\000\
\000\000\000\000\000\000\054\001\000\000\081\000\081\000\081\000\
\081\000\081\000\081\000\000\000\000\000\081\000\081\000\081\000\
\081\000\000\000\000\000\000\000\099\000\081\000\000\000\081\000\
\081\000\000\000\000\000\000\000\000\000\000\000\000\000\081\000\
\000\000\081\000\093\000\093\000\000\000\000\000\093\000\093\000\
\093\000\000\000\093\000\093\000\093\000\093\000\093\000\000\000\
\093\000\093\000\093\000\093\000\000\000\093\000\093\000\093\000\
\000\000\000\000\093\000\093\000\093\000\093\000\000\000\000\000\
\093\000\093\000\093\000\000\000\000\000\000\000\093\000\133\000\
\134\000\000\000\000\000\000\000\135\000\136\000\137\000\138\000\
\139\000\140\000\141\000\142\000\000\000\000\000\093\000\093\000\
\093\000\093\000\093\000\093\000\000\000\000\000\093\000\093\000\
\093\000\093\000\000\000\000\000\000\000\088\000\093\000\000\000\
\093\000\093\000\000\000\000\000\000\000\000\000\000\000\000\000\
\093\000\143\000\093\000\097\000\097\000\000\000\000\000\097\000\
\097\000\097\000\000\000\097\000\097\000\097\000\097\000\097\000\
\000\000\097\000\097\000\097\000\097\000\000\000\097\000\097\000\
\097\000\000\000\189\000\097\000\097\000\097\000\097\000\000\000\
\000\000\097\000\097\000\097\000\000\000\197\000\000\000\097\000\
\133\000\134\000\000\000\000\000\000\000\135\000\136\000\137\000\
\138\000\139\000\140\000\141\000\142\000\198\000\000\000\097\000\
\097\000\097\000\097\000\097\000\097\000\000\000\000\000\097\000\
\097\000\097\000\097\000\000\000\000\000\000\000\095\000\097\000\
\000\000\097\000\097\000\000\000\000\000\000\000\000\000\000\000\
\000\000\097\000\143\000\097\000\098\000\098\000\000\000\000\000\
\098\000\098\000\098\000\000\000\098\000\098\000\098\000\098\000\
\098\000\000\000\098\000\098\000\098\000\098\000\000\000\098\000\
\098\000\098\000\000\000\000\000\098\000\098\000\098\000\098\000\
\000\000\000\000\098\000\098\000\098\000\000\000\215\000\000\000\
\098\000\133\000\134\000\000\000\000\000\000\000\135\000\136\000\
\137\000\138\000\139\000\140\000\141\000\142\000\198\000\000\000\
\098\000\098\000\098\000\098\000\098\000\098\000\000\000\087\000\
\098\000\098\000\098\000\098\000\000\000\000\000\000\000\000\000\
\098\000\000\000\098\000\098\000\000\000\000\000\000\000\000\000\
\000\000\000\000\098\000\143\000\098\000\099\000\099\000\000\000\
\000\000\099\000\099\000\099\000\000\000\099\000\099\000\099\000\
\099\000\099\000\000\000\099\000\099\000\099\000\099\000\000\000\
\099\000\099\000\099\000\000\000\000\000\099\000\099\000\099\000\
\099\000\000\000\000\000\099\000\099\000\099\000\133\000\134\000\
\000\000\099\000\000\000\135\000\136\000\137\000\138\000\139\000\
\140\000\141\000\142\000\017\001\000\000\000\000\000\000\018\001\
\091\000\099\000\099\000\099\000\099\000\099\000\099\000\000\000\
\000\000\099\000\099\000\099\000\099\000\000\000\000\000\000\000\
\000\000\099\000\000\000\099\000\099\000\000\000\000\000\000\000\
\143\000\000\000\000\000\099\000\000\000\099\000\088\000\088\000\
\000\000\000\000\088\000\088\000\088\000\000\000\088\000\088\000\
\088\000\088\000\088\000\000\000\088\000\088\000\088\000\088\000\
\000\000\088\000\088\000\088\000\000\000\000\000\088\000\088\000\
\088\000\088\000\000\000\000\000\088\000\088\000\088\000\000\000\
\000\000\000\000\088\000\133\000\134\000\000\000\000\000\000\000\
\135\000\090\000\137\000\138\000\139\000\140\000\141\000\142\000\
\000\000\000\000\088\000\088\000\088\000\088\000\088\000\088\000\
\000\000\000\000\088\000\088\000\088\000\088\000\000\000\000\000\
\000\000\000\000\088\000\000\000\088\000\088\000\000\000\000\000\
\000\000\000\000\000\000\000\000\088\000\143\000\088\000\095\000\
\095\000\000\000\000\000\095\000\095\000\095\000\000\000\095\000\
\095\000\095\000\095\000\095\000\000\000\095\000\095\000\095\000\
\095\000\000\000\095\000\095\000\095\000\000\000\000\000\095\000\
\095\000\095\000\095\000\000\000\000\000\095\000\095\000\095\000\
\133\000\134\000\089\000\095\000\000\000\135\000\136\000\137\000\
\138\000\139\000\140\000\141\000\142\000\000\000\000\000\000\000\
\000\000\000\000\000\000\095\000\095\000\095\000\095\000\095\000\
\095\000\000\000\000\000\095\000\095\000\095\000\095\000\000\000\
\026\001\000\000\000\000\095\000\000\000\095\000\095\000\000\000\
\087\000\087\000\143\000\000\000\087\000\095\000\087\000\095\000\
\087\000\087\000\087\000\087\000\087\000\000\000\087\000\087\000\
\087\000\087\000\000\000\087\000\087\000\087\000\000\000\000\000\
\087\000\087\000\087\000\087\000\000\000\000\000\087\000\087\000\
\087\000\133\000\134\000\086\000\087\000\000\000\135\000\136\000\
\137\000\138\000\139\000\140\000\141\000\142\000\198\000\000\000\
\000\000\000\000\000\000\000\000\087\000\087\000\087\000\087\000\
\087\000\087\000\000\000\000\000\087\000\087\000\087\000\087\000\
\000\000\000\000\000\000\000\000\087\000\000\000\087\000\087\000\
\000\000\091\000\091\000\143\000\000\000\000\000\087\000\091\000\
\087\000\091\000\091\000\091\000\091\000\091\000\000\000\091\000\
\091\000\091\000\091\000\000\000\091\000\091\000\091\000\000\000\
\000\000\091\000\091\000\091\000\091\000\000\000\000\000\091\000\
\091\000\091\000\133\000\134\000\084\000\091\000\000\000\135\000\
\136\000\137\000\138\000\139\000\140\000\141\000\142\000\000\000\
\000\000\000\000\000\000\029\001\000\000\091\000\091\000\091\000\
\091\000\091\000\091\000\000\000\000\000\091\000\091\000\091\000\
\091\000\000\000\000\000\000\000\000\000\091\000\000\000\091\000\
\091\000\000\000\090\000\090\000\143\000\000\000\000\000\091\000\
\090\000\091\000\090\000\090\000\090\000\090\000\090\000\000\000\
\090\000\090\000\000\000\090\000\000\000\090\000\090\000\090\000\
\000\000\000\000\090\000\090\000\090\000\090\000\092\000\000\000\
\090\000\090\000\090\000\133\000\134\000\000\000\090\000\000\000\
\135\000\136\000\137\000\138\000\139\000\140\000\141\000\142\000\
\036\001\000\000\000\000\000\000\000\000\000\000\090\000\090\000\
\090\000\090\000\090\000\090\000\000\000\000\000\090\000\090\000\
\090\000\090\000\000\000\000\000\000\000\000\000\090\000\000\000\
\090\000\090\000\000\000\089\000\089\000\143\000\000\000\000\000\
\090\000\089\000\090\000\089\000\089\000\089\000\089\000\089\000\
\000\000\089\000\000\000\000\000\089\000\000\000\089\000\089\000\
\089\000\000\000\000\000\089\000\089\000\089\000\089\000\083\000\
\000\000\089\000\089\000\089\000\133\000\134\000\000\000\089\000\
\000\000\135\000\136\000\137\000\138\000\139\000\140\000\141\000\
\142\000\000\000\000\000\000\000\000\000\042\001\000\000\089\000\
\089\000\089\000\089\000\089\000\089\000\000\000\000\000\089\000\
\089\000\089\000\089\000\000\000\000\000\000\000\000\000\089\000\
\000\000\089\000\089\000\000\000\086\000\086\000\143\000\000\000\
\000\000\089\000\086\000\089\000\086\000\086\000\086\000\086\000\
\086\000\000\000\000\000\000\000\000\000\086\000\000\000\086\000\
\086\000\086\000\000\000\000\000\086\000\086\000\086\000\086\000\
\082\000\000\000\086\000\086\000\086\000\133\000\134\000\000\000\
\086\000\000\000\135\000\136\000\137\000\138\000\139\000\140\000\
\141\000\142\000\051\001\000\000\000\000\000\000\000\000\000\000\
\086\000\086\000\086\000\086\000\086\000\086\000\000\000\000\000\
\086\000\086\000\086\000\086\000\000\000\000\000\000\000\000\000\
\086\000\000\000\086\000\086\000\000\000\084\000\084\000\143\000\
\000\000\000\000\086\000\084\000\086\000\084\000\000\000\084\000\
\084\000\084\000\000\000\000\000\000\000\000\000\084\000\000\000\
\084\000\084\000\084\000\000\000\000\000\084\000\084\000\084\000\
\084\000\102\000\000\000\084\000\084\000\084\000\000\000\000\000\
\000\000\084\000\133\000\134\000\000\000\000\000\000\000\135\000\
\136\000\137\000\138\000\139\000\140\000\141\000\142\000\000\000\
\000\000\084\000\084\000\084\000\084\000\084\000\084\000\000\000\
\173\000\084\000\084\000\084\000\084\000\000\000\000\000\092\000\
\092\000\084\000\000\000\084\000\084\000\092\000\000\000\092\000\
\000\000\092\000\092\000\084\000\143\000\084\000\000\000\000\000\
\092\000\000\000\092\000\092\000\092\000\000\000\000\000\092\000\
\092\000\092\000\092\000\000\000\000\000\092\000\092\000\092\000\
\093\000\000\000\000\000\092\000\000\000\000\000\012\000\000\000\
\000\000\000\000\000\000\000\000\000\000\108\000\000\000\000\000\
\000\000\000\000\000\000\092\000\092\000\092\000\092\000\092\000\
\092\000\000\000\000\000\092\000\092\000\092\000\092\000\000\000\
\000\000\000\000\000\000\092\000\000\000\092\000\092\000\000\000\
\083\000\083\000\000\000\000\000\000\000\092\000\083\000\092\000\
\083\000\000\000\083\000\083\000\000\000\000\000\000\000\000\000\
\000\000\083\000\000\000\083\000\083\000\083\000\000\000\000\000\
\083\000\083\000\083\000\083\000\000\000\064\000\083\000\083\000\
\083\000\000\000\133\000\134\000\083\000\000\000\000\000\135\000\
\000\000\165\000\138\000\139\000\140\000\141\000\142\000\000\000\
\000\000\000\000\000\000\000\000\083\000\083\000\083\000\083\000\
\083\000\083\000\000\000\000\000\083\000\083\000\083\000\083\000\
\190\000\000\000\192\000\000\000\083\000\000\000\083\000\083\000\
\000\000\082\000\082\000\000\000\143\000\000\000\083\000\082\000\
\083\000\082\000\000\000\082\000\000\000\000\000\000\000\000\000\
\000\000\000\000\082\000\000\000\082\000\082\000\082\000\060\000\
\000\000\082\000\082\000\082\000\082\000\000\000\000\000\082\000\
\082\000\082\000\000\000\133\000\134\000\082\000\000\000\000\000\
\000\000\000\000\233\000\235\000\139\000\140\000\141\000\142\000\
\000\000\000\000\241\000\000\000\000\000\082\000\082\000\082\000\
\082\000\082\000\082\000\000\000\000\000\082\000\082\000\082\000\
\082\000\000\000\000\000\000\000\254\000\082\000\000\000\082\000\
\082\000\000\000\102\000\102\000\000\000\143\000\000\000\082\000\
\102\000\082\000\102\000\000\000\000\000\000\000\000\000\000\000\
\000\000\056\000\000\000\102\000\000\000\102\000\102\000\102\000\
\000\000\000\000\102\000\102\000\102\000\102\000\000\000\000\000\
\102\000\102\000\102\000\133\000\134\000\000\000\102\000\000\000\
\135\000\136\000\137\000\138\000\139\000\140\000\141\000\142\000\
\000\000\000\000\000\000\000\000\000\000\000\000\102\000\102\000\
\102\000\102\000\102\000\102\000\000\000\000\000\102\000\102\000\
\102\000\102\000\000\000\000\000\000\000\000\000\102\000\000\000\
\102\000\102\000\233\000\233\000\233\000\143\000\000\000\011\000\
\102\000\035\001\102\000\133\000\134\000\013\000\000\000\038\001\
\135\000\136\000\137\000\138\000\139\000\140\000\141\000\142\000\
\133\000\134\000\014\000\063\000\000\000\000\000\233\000\015\000\
\016\000\139\000\000\000\141\000\142\000\017\000\018\000\019\000\
\000\000\000\000\000\000\000\000\000\000\057\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\143\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\021\000\022\000\023\000\
\024\000\000\000\143\000\000\000\026\000\000\000\064\000\064\000\
\000\000\000\000\000\000\000\000\064\000\027\000\064\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\029\000\
\000\000\064\000\064\000\000\000\000\000\061\000\064\000\064\000\
\000\000\064\000\000\000\000\000\064\000\064\000\064\000\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\064\000\064\000\064\000\064\000\064\000\064\000\
\000\000\000\000\064\000\064\000\064\000\064\000\000\000\000\000\
\060\000\060\000\064\000\000\000\064\000\064\000\060\000\000\000\
\060\000\000\000\000\000\000\000\064\000\000\000\064\000\000\000\
\000\000\000\000\000\000\060\000\060\000\000\000\000\000\057\000\
\060\000\060\000\000\000\060\000\000\000\000\000\060\000\060\000\
\060\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\060\000\060\000\060\000\060\000\
\060\000\060\000\000\000\000\000\060\000\060\000\060\000\060\000\
\000\000\000\000\056\000\056\000\060\000\000\000\060\000\060\000\
\056\000\000\000\056\000\000\000\000\000\000\000\060\000\000\000\
\060\000\000\000\000\000\000\000\000\000\056\000\056\000\000\000\
\000\000\059\000\056\000\056\000\000\000\056\000\000\000\000\000\
\056\000\056\000\056\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\056\000\056\000\
\056\000\056\000\056\000\056\000\000\000\000\000\056\000\056\000\
\056\000\056\000\000\000\000\000\000\000\000\000\056\000\000\000\
\056\000\056\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\000\000\000\056\000\000\000\063\000\063\000\000\000\000\000\
\000\000\000\000\063\000\055\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\063\000\
\063\000\000\000\000\000\000\000\063\000\063\000\000\000\063\000\
\000\000\000\000\063\000\063\000\063\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\063\000\063\000\063\000\063\000\063\000\063\000\000\000\000\000\
\063\000\063\000\063\000\063\000\000\000\000\000\061\000\061\000\
\063\000\000\000\063\000\063\000\061\000\062\000\061\000\000\000\
\000\000\000\000\063\000\000\000\063\000\000\000\000\000\000\000\
\000\000\061\000\061\000\000\000\000\000\000\000\061\000\061\000\
\000\000\061\000\000\000\000\000\061\000\061\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\061\000\061\000\061\000\061\000\061\000\061\000\
\000\000\000\000\061\000\061\000\061\000\061\000\000\000\000\000\
\057\000\057\000\061\000\000\000\061\000\061\000\057\000\058\000\
\057\000\000\000\000\000\000\000\061\000\000\000\061\000\000\000\
\000\000\000\000\000\000\057\000\057\000\000\000\000\000\000\000\
\057\000\057\000\000\000\057\000\000\000\000\000\057\000\057\000\
\057\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\057\000\057\000\057\000\057\000\
\057\000\057\000\000\000\000\000\057\000\057\000\057\000\057\000\
\000\000\053\000\059\000\059\000\057\000\000\000\057\000\057\000\
\059\000\000\000\059\000\000\000\000\000\000\000\057\000\000\000\
\057\000\000\000\000\000\000\000\000\000\059\000\059\000\000\000\
\000\000\000\000\059\000\059\000\000\000\059\000\000\000\000\000\
\059\000\059\000\059\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\059\000\059\000\
\059\000\059\000\059\000\059\000\000\000\000\000\059\000\059\000\
\059\000\059\000\000\000\035\000\055\000\055\000\059\000\000\000\
\059\000\059\000\055\000\000\000\055\000\000\000\000\000\000\000\
\059\000\000\000\059\000\000\000\000\000\000\000\000\000\055\000\
\055\000\000\000\000\000\000\000\055\000\055\000\000\000\055\000\
\000\000\000\000\055\000\055\000\055\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\055\000\055\000\055\000\055\000\055\000\055\000\000\000\000\000\
\055\000\055\000\055\000\055\000\000\000\042\000\000\000\062\000\
\055\000\000\000\055\000\055\000\062\000\000\000\062\000\000\000\
\000\000\000\000\055\000\000\000\055\000\000\000\000\000\000\000\
\000\000\062\000\062\000\000\000\000\000\000\000\062\000\062\000\
\000\000\062\000\000\000\000\000\000\000\062\000\062\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\062\000\062\000\062\000\062\000\062\000\062\000\
\000\000\000\000\062\000\062\000\062\000\062\000\000\000\051\000\
\000\000\058\000\062\000\000\000\062\000\062\000\058\000\000\000\
\058\000\000\000\000\000\000\000\062\000\000\000\062\000\000\000\
\000\000\000\000\000\000\058\000\058\000\000\000\000\000\000\000\
\058\000\058\000\000\000\058\000\000\000\000\000\000\000\058\000\
\058\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\058\000\058\000\058\000\058\000\
\058\000\058\000\053\000\053\000\058\000\058\000\058\000\058\000\
\053\000\000\000\053\000\000\000\058\000\000\000\058\000\058\000\
\000\000\000\000\000\000\000\000\000\000\053\000\058\000\000\000\
\058\000\000\000\053\000\053\000\000\000\053\000\000\000\000\000\
\053\000\053\000\053\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\120\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\053\000\
\053\000\053\000\053\000\053\000\000\000\000\000\000\000\053\000\
\053\000\053\000\000\000\000\000\035\000\035\000\000\000\000\000\
\053\000\053\000\035\000\000\000\035\000\000\000\000\000\000\000\
\053\000\000\000\053\000\000\000\000\000\010\000\000\000\035\000\
\000\000\000\000\000\000\000\000\035\000\035\000\000\000\035\000\
\000\000\000\000\035\000\035\000\035\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\035\000\035\000\035\000\035\000\035\000\035\000\000\000\000\000\
\013\000\035\000\035\000\035\000\000\000\000\000\042\000\042\000\
\000\000\000\000\035\000\035\000\042\000\000\000\042\000\000\000\
\000\000\000\000\035\000\000\000\035\000\000\000\000\000\000\000\
\000\000\042\000\000\000\000\000\000\000\000\000\042\000\042\000\
\000\000\000\000\000\000\000\000\042\000\042\000\042\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\116\000\042\000\042\000\042\000\042\000\042\000\042\000\
\000\000\000\000\000\000\042\000\042\000\042\000\000\000\000\000\
\051\000\051\000\000\000\000\000\042\000\042\000\051\000\000\000\
\051\000\000\000\000\000\000\000\042\000\000\000\042\000\000\000\
\000\000\000\000\000\000\051\000\000\000\000\000\000\000\000\000\
\051\000\051\000\115\000\051\000\000\000\000\000\051\000\051\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\000\038\000\000\000\000\000\000\000\000\000\038\000\
\000\000\038\000\000\000\000\000\051\000\051\000\051\000\051\000\
\051\000\051\000\000\000\000\000\038\000\051\000\051\000\051\000\
\000\000\038\000\038\000\014\000\038\000\000\000\051\000\038\000\
\038\000\038\000\000\000\000\000\000\000\000\000\051\000\000\000\
\051\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\038\000\038\000\
\038\000\038\000\038\000\120\000\120\000\000\000\038\000\000\000\
\000\000\120\000\000\000\000\000\120\000\000\000\012\000\038\000\
\038\000\000\000\000\000\000\000\120\000\000\000\120\000\038\000\
\000\000\038\000\000\000\120\000\120\000\000\000\000\000\000\000\
\000\000\120\000\120\000\120\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\120\000\120\000\120\000\120\000\120\000\000\000\000\000\000\000\
\120\000\010\000\010\000\000\000\000\000\000\000\010\000\010\000\
\012\000\120\000\000\000\000\000\010\000\010\000\010\000\000\000\
\000\000\120\000\000\000\120\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\000\000\010\000\010\000\010\000\010\000\010\000\
\000\000\000\000\010\000\010\000\013\000\013\000\000\000\000\000\
\000\000\013\000\013\000\012\000\010\000\000\000\000\000\013\000\
\013\000\013\000\000\000\000\000\010\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\013\000\013\000\
\013\000\013\000\013\000\116\000\000\000\013\000\013\000\000\000\
\116\000\000\000\000\000\116\000\012\000\000\000\000\000\013\000\
\000\000\000\000\000\000\116\000\000\000\116\000\000\000\013\000\
\000\000\013\000\116\000\116\000\000\000\000\000\000\000\000\000\
\000\000\116\000\116\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\115\000\000\000\000\000\000\000\
\000\000\115\000\000\000\000\000\115\000\000\000\000\000\116\000\
\116\000\116\000\116\000\116\000\115\000\000\000\115\000\116\000\
\008\000\000\000\000\000\115\000\115\000\000\000\000\000\000\000\
\116\000\000\000\115\000\115\000\000\000\000\000\000\000\000\000\
\116\000\000\000\116\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\000\000\
\115\000\115\000\115\000\115\000\115\000\000\000\000\000\014\000\
\115\000\000\000\000\000\012\000\014\000\014\000\000\000\000\000\
\000\000\115\000\014\000\014\000\014\000\000\000\000\000\000\000\
\000\000\115\000\000\000\115\000\000\000\000\000\000\000\011\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\014\000\014\000\014\000\014\000\014\000\000\000\000\000\
\014\000\014\000\014\000\000\000\006\000\000\000\000\000\015\000\
\016\000\000\000\014\000\000\000\000\000\017\000\018\000\019\000\
\000\000\000\000\014\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\020\000\021\000\022\000\023\000\
\024\000\011\000\000\000\025\000\026\000\000\000\000\000\013\000\
\000\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\028\000\000\000\029\000\
\000\000\015\000\016\000\000\000\000\000\000\000\000\000\017\000\
\018\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\020\000\021\000\
\022\000\023\000\024\000\000\000\000\000\036\000\026\000\014\000\
\000\000\000\000\000\000\000\000\015\000\016\000\000\000\027\000\
\000\000\000\000\017\000\018\000\019\000\000\000\000\000\028\000\
\000\000\029\000\000\000\000\000\000\000\011\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\175\000\020\000\021\000\022\000\023\000\024\000\000\000\000\000\
\014\000\026\000\000\000\000\000\000\000\015\000\016\000\000\000\
\000\000\000\000\027\000\017\000\018\000\019\000\000\000\000\000\
\000\000\000\000\028\000\000\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\020\000\021\000\022\000\023\000\024\000\008\000\
\000\000\186\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\008\000\000\000\000\000\000\000\
\000\000\008\000\008\000\028\000\000\000\029\000\000\000\008\000\
\008\000\008\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\
\000\000\000\000\013\000\000\000\000\000\000\000\008\000\008\000\
\008\000\008\000\008\000\000\000\000\000\008\000\008\000\014\000\
\000\000\000\000\000\000\000\000\015\000\016\000\000\000\008\000\
\000\000\000\000\017\000\018\000\019\000\000\000\000\000\008\000\
\000\000\008\000\042\000\000\000\000\000\006\000\043\000\000\000\
\000\000\000\000\000\000\006\000\000\000\044\000\000\000\000\000\
\000\000\020\000\021\000\022\000\023\000\024\000\000\000\000\000\
\006\000\026\000\000\000\000\000\000\000\006\000\006\000\000\000\
\000\000\000\000\027\000\006\000\006\000\006\000\000\000\000\000\
\000\000\000\000\028\000\000\000\029\000\045\000\046\000\047\000\
\000\000\048\000\049\000\050\000\051\000\000\000\000\000\000\000\
\000\000\000\000\006\000\006\000\006\000\006\000\006\000\000\000\
\000\000\006\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\052\000\053\000\000\000\000\000\
\054\000\055\000\056\000\057\000\058\000\006\000\011\000\000\000\
\000\000\000\000\133\000\134\000\013\000\000\000\000\000\135\000\
\136\000\137\000\138\000\139\000\140\000\141\000\142\000\000\000\
\000\000\014\000\011\000\000\000\000\000\000\000\089\000\016\000\
\013\000\000\000\000\000\000\000\017\000\000\000\090\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\016\000\143\000\000\000\000\000\000\000\
\017\000\000\000\000\000\091\000\021\000\022\000\023\000\092\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\027\000\000\000\000\000\000\000\
\021\000\022\000\023\000\000\000\028\000\000\000\029\000\026\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\029\000"

let yycheck = "\036\000\
\000\000\073\000\165\000\002\000\022\000\023\000\004\000\003\000\
\069\000\020\000\003\000\062\000\008\000\001\001\007\001\007\001\
\014\000\013\000\016\000\156\000\081\001\000\000\031\000\005\001\
\006\001\021\000\001\001\020\001\020\001\027\000\110\000\040\000\
\014\001\059\001\020\001\023\001\022\001\006\001\013\001\119\000\
\014\001\067\001\001\001\081\001\042\000\014\001\044\000\056\000\
\007\001\001\001\048\000\049\000\050\000\051\000\065\000\081\001\
\031\001\081\001\069\000\005\001\006\001\020\001\055\001\055\001\
\062\000\047\001\025\001\026\001\014\001\062\001\062\001\017\001\
\031\001\047\001\033\001\081\001\002\001\149\000\047\001\031\001\
\091\000\000\000\020\001\155\000\156\000\222\000\001\001\096\000\
\081\001\081\000\082\000\001\001\018\001\081\001\002\001\054\001\
\055\001\056\001\057\001\058\001\172\000\047\001\101\000\062\001\
\068\001\177\000\013\001\014\001\015\001\108\000\018\001\069\001\
\071\001\112\000\110\000\113\000\031\001\110\000\001\001\118\000\
\079\001\031\001\081\001\119\000\100\000\020\001\119\000\022\001\
\104\000\021\001\013\001\063\001\064\001\021\001\041\001\133\000\
\134\000\135\000\136\000\137\000\138\000\139\000\140\000\141\000\
\142\000\143\000\144\000\198\000\031\001\186\000\222\000\075\001\
\076\001\082\001\001\001\001\001\217\000\001\001\230\000\231\000\
\020\001\022\001\000\000\181\000\081\001\005\001\006\001\001\001\
\009\001\081\001\010\001\169\000\081\001\171\000\014\001\015\001\
\016\001\017\001\024\001\009\001\178\000\179\000\002\001\029\001\
\030\001\002\001\082\001\010\001\010\001\021\001\069\001\080\001\
\002\001\039\001\040\001\041\001\042\001\018\001\082\001\002\001\
\198\000\002\001\018\001\018\001\082\001\002\001\217\000\047\001\
\002\001\018\001\081\001\010\001\010\001\029\001\020\001\060\001\
\079\001\013\001\080\001\065\001\066\001\013\001\002\001\082\001\
\002\001\018\001\042\001\073\001\074\001\002\001\019\001\229\000\
\078\001\018\001\232\000\081\001\018\001\028\001\006\001\082\001\
\238\000\240\000\002\001\000\000\242\000\244\000\245\000\082\001\
\031\001\247\000\000\000\082\001\010\001\072\001\000\000\002\001\
\000\000\175\000\002\001\166\000\046\001\005\001\006\001\007\001\
\002\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\017\001\020\001\021\001\022\001\002\001\
\002\001\025\001\026\001\027\001\028\001\025\001\002\001\010\001\
\032\001\033\001\002\001\034\001\002\001\037\001\036\001\018\001\
\161\000\051\001\255\255\255\255\255\255\255\255\255\255\047\001\
\255\255\255\255\255\255\050\001\255\255\053\001\054\001\055\001\
\056\001\057\001\058\001\255\255\255\255\061\001\062\001\063\001\
\064\001\255\255\255\255\255\255\000\000\069\001\255\255\071\001\
\072\001\255\255\255\255\255\255\255\255\255\255\255\255\079\001\
\255\255\081\001\001\001\002\001\255\255\255\255\005\001\006\001\
\007\001\255\255\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\017\001\018\001\255\255\020\001\021\001\022\001\
\255\255\255\255\025\001\026\001\027\001\028\001\255\255\255\255\
\031\001\032\001\033\001\255\255\255\255\255\255\037\001\005\001\
\006\001\255\255\255\255\255\255\010\001\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\061\001\062\001\
\063\001\064\001\255\255\255\255\255\255\000\000\069\001\255\255\
\071\001\072\001\255\255\255\255\255\255\255\255\255\255\255\255\
\079\001\047\001\081\001\001\001\002\001\255\255\255\255\005\001\
\006\001\007\001\255\255\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\016\001\017\001\018\001\255\255\020\001\021\001\
\022\001\255\255\072\001\025\001\026\001\027\001\028\001\255\255\
\255\255\031\001\032\001\033\001\255\255\002\001\255\255\037\001\
\005\001\006\001\255\255\255\255\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\053\001\
\054\001\055\001\056\001\057\001\058\001\255\255\255\255\061\001\
\062\001\063\001\064\001\255\255\255\255\255\255\000\000\069\001\
\255\255\071\001\072\001\255\255\255\255\255\255\255\255\255\255\
\255\255\079\001\047\001\081\001\001\001\002\001\255\255\255\255\
\005\001\006\001\007\001\255\255\009\001\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\017\001\018\001\255\255\020\001\
\021\001\022\001\255\255\255\255\025\001\026\001\027\001\028\001\
\255\255\255\255\031\001\032\001\033\001\255\255\002\001\255\255\
\037\001\005\001\006\001\255\255\255\255\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\000\000\
\061\001\062\001\063\001\064\001\255\255\255\255\255\255\255\255\
\069\001\255\255\071\001\072\001\255\255\255\255\255\255\255\255\
\255\255\255\255\079\001\047\001\081\001\001\001\002\001\255\255\
\255\255\005\001\006\001\007\001\255\255\009\001\010\001\011\001\
\012\001\013\001\255\255\015\001\016\001\017\001\018\001\255\255\
\020\001\021\001\022\001\255\255\255\255\025\001\026\001\027\001\
\028\001\255\255\255\255\031\001\032\001\033\001\005\001\006\001\
\255\255\037\001\255\255\010\001\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\255\255\255\255\255\255\022\001\
\000\000\053\001\054\001\055\001\056\001\057\001\058\001\255\255\
\255\255\061\001\062\001\063\001\064\001\255\255\255\255\255\255\
\255\255\069\001\255\255\071\001\072\001\255\255\255\255\255\255\
\047\001\255\255\255\255\079\001\255\255\081\001\001\001\002\001\
\255\255\255\255\005\001\006\001\007\001\255\255\009\001\010\001\
\011\001\012\001\013\001\255\255\015\001\016\001\017\001\018\001\
\255\255\020\001\021\001\022\001\255\255\255\255\025\001\026\001\
\027\001\028\001\255\255\255\255\031\001\032\001\033\001\255\255\
\255\255\255\255\037\001\005\001\006\001\255\255\255\255\255\255\
\010\001\000\000\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\053\001\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\061\001\062\001\063\001\064\001\255\255\255\255\
\255\255\255\255\069\001\255\255\071\001\072\001\255\255\255\255\
\255\255\255\255\255\255\255\255\079\001\047\001\081\001\001\001\
\002\001\255\255\255\255\005\001\006\001\007\001\255\255\009\001\
\010\001\011\001\012\001\013\001\255\255\015\001\016\001\017\001\
\018\001\255\255\020\001\021\001\022\001\255\255\255\255\025\001\
\026\001\027\001\028\001\255\255\255\255\031\001\032\001\033\001\
\005\001\006\001\000\000\037\001\255\255\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\255\255\255\255\255\255\
\255\255\255\255\255\255\053\001\054\001\055\001\056\001\057\001\
\058\001\255\255\255\255\061\001\062\001\063\001\064\001\255\255\
\037\001\255\255\255\255\069\001\255\255\071\001\072\001\255\255\
\001\001\002\001\047\001\255\255\005\001\079\001\007\001\081\001\
\009\001\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\017\001\018\001\255\255\020\001\021\001\022\001\255\255\255\255\
\025\001\026\001\027\001\028\001\255\255\255\255\031\001\032\001\
\033\001\005\001\006\001\000\000\037\001\255\255\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\255\255\255\255\061\001\062\001\063\001\064\001\
\255\255\255\255\255\255\255\255\069\001\255\255\071\001\072\001\
\255\255\001\001\002\001\047\001\255\255\255\255\079\001\007\001\
\081\001\009\001\010\001\011\001\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\255\255\020\001\021\001\022\001\255\255\
\255\255\025\001\026\001\027\001\028\001\255\255\255\255\031\001\
\032\001\033\001\005\001\006\001\000\000\037\001\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\255\255\255\255\022\001\255\255\053\001\054\001\055\001\
\056\001\057\001\058\001\255\255\255\255\061\001\062\001\063\001\
\064\001\255\255\255\255\255\255\255\255\069\001\255\255\071\001\
\072\001\255\255\001\001\002\001\047\001\255\255\255\255\079\001\
\007\001\081\001\009\001\010\001\011\001\012\001\013\001\255\255\
\015\001\016\001\255\255\018\001\255\255\020\001\021\001\022\001\
\255\255\255\255\025\001\026\001\027\001\028\001\000\000\255\255\
\031\001\032\001\033\001\005\001\006\001\255\255\037\001\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\061\001\062\001\
\063\001\064\001\255\255\255\255\255\255\255\255\069\001\255\255\
\071\001\072\001\255\255\001\001\002\001\047\001\255\255\255\255\
\079\001\007\001\081\001\009\001\010\001\011\001\012\001\013\001\
\255\255\015\001\255\255\255\255\018\001\255\255\020\001\021\001\
\022\001\255\255\255\255\025\001\026\001\027\001\028\001\000\000\
\255\255\031\001\032\001\033\001\005\001\006\001\255\255\037\001\
\255\255\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\255\255\255\255\255\255\255\255\022\001\255\255\053\001\
\054\001\055\001\056\001\057\001\058\001\255\255\255\255\061\001\
\062\001\063\001\064\001\255\255\255\255\255\255\255\255\069\001\
\255\255\071\001\072\001\255\255\001\001\002\001\047\001\255\255\
\255\255\079\001\007\001\081\001\009\001\010\001\011\001\012\001\
\013\001\255\255\255\255\255\255\255\255\018\001\255\255\020\001\
\021\001\022\001\255\255\255\255\025\001\026\001\027\001\028\001\
\000\000\255\255\031\001\032\001\033\001\005\001\006\001\255\255\
\037\001\255\255\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\061\001\062\001\063\001\064\001\255\255\255\255\255\255\255\255\
\069\001\255\255\071\001\072\001\255\255\001\001\002\001\047\001\
\255\255\255\255\079\001\007\001\081\001\009\001\255\255\011\001\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\255\255\
\020\001\021\001\022\001\255\255\255\255\025\001\026\001\027\001\
\028\001\000\000\255\255\031\001\032\001\033\001\255\255\255\255\
\255\255\037\001\005\001\006\001\255\255\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\053\001\054\001\055\001\056\001\057\001\058\001\255\255\
\027\001\061\001\062\001\063\001\064\001\255\255\255\255\001\001\
\002\001\069\001\255\255\071\001\072\001\007\001\255\255\009\001\
\255\255\011\001\012\001\079\001\047\001\081\001\255\255\255\255\
\018\001\255\255\020\001\021\001\022\001\255\255\255\255\025\001\
\026\001\027\001\028\001\255\255\255\255\031\001\032\001\033\001\
\011\000\255\255\255\255\037\001\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\024\000\255\255\255\255\
\255\255\255\255\255\255\053\001\054\001\055\001\056\001\057\001\
\058\001\255\255\255\255\061\001\062\001\063\001\064\001\255\255\
\255\255\255\255\255\255\069\001\255\255\071\001\072\001\255\255\
\001\001\002\001\255\255\255\255\255\255\079\001\007\001\081\001\
\009\001\255\255\011\001\012\001\255\255\255\255\255\255\255\255\
\255\255\018\001\255\255\020\001\021\001\022\001\255\255\255\255\
\025\001\026\001\027\001\028\001\255\255\000\000\031\001\032\001\
\033\001\255\255\005\001\006\001\037\001\255\255\255\255\010\001\
\255\255\092\000\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\255\255\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\255\255\255\255\061\001\062\001\063\001\064\001\
\115\000\255\255\117\000\255\255\069\001\255\255\071\001\072\001\
\255\255\001\001\002\001\255\255\047\001\255\255\079\001\007\001\
\081\001\009\001\255\255\011\001\255\255\255\255\255\255\255\255\
\255\255\255\255\018\001\255\255\020\001\021\001\022\001\000\000\
\255\255\025\001\026\001\027\001\028\001\255\255\255\255\031\001\
\032\001\033\001\255\255\005\001\006\001\037\001\255\255\255\255\
\255\255\255\255\165\000\166\000\014\001\015\001\016\001\017\001\
\255\255\255\255\173\000\255\255\255\255\053\001\054\001\055\001\
\056\001\057\001\058\001\255\255\255\255\061\001\062\001\063\001\
\064\001\255\255\255\255\255\255\191\000\069\001\255\255\071\001\
\072\001\255\255\001\001\002\001\255\255\047\001\255\255\079\001\
\007\001\081\001\009\001\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\018\001\255\255\020\001\021\001\022\001\
\255\255\255\255\025\001\026\001\027\001\028\001\255\255\255\255\
\031\001\032\001\033\001\005\001\006\001\255\255\037\001\255\255\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\061\001\062\001\
\063\001\064\001\255\255\255\255\255\255\255\255\069\001\255\255\
\071\001\072\001\013\001\014\001\015\001\047\001\255\255\001\001\
\079\001\020\001\081\001\005\001\006\001\007\001\255\255\026\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\005\001\006\001\020\001\000\000\255\255\255\255\041\001\025\001\
\026\001\014\001\255\255\016\001\017\001\031\001\032\001\033\001\
\255\255\255\255\255\255\255\255\255\255\056\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\047\001\255\255\255\255\
\255\255\255\255\255\255\255\255\054\001\055\001\056\001\057\001\
\058\001\255\255\047\001\255\255\062\001\255\255\001\001\002\001\
\255\255\255\255\255\255\255\255\007\001\071\001\009\001\255\255\
\255\255\255\255\255\255\255\255\255\255\079\001\255\255\081\001\
\255\255\020\001\021\001\255\255\255\255\000\000\025\001\026\001\
\255\255\028\001\255\255\255\255\031\001\032\001\033\001\001\000\
\002\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\061\001\062\001\063\001\064\001\255\255\255\255\
\001\001\002\001\069\001\255\255\071\001\072\001\007\001\255\255\
\009\001\255\255\255\255\255\255\079\001\255\255\081\001\255\255\
\255\255\255\255\255\255\020\001\021\001\255\255\255\255\000\000\
\025\001\026\001\255\255\028\001\255\255\255\255\031\001\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\255\255\255\255\061\001\062\001\063\001\064\001\
\255\255\255\255\001\001\002\001\069\001\255\255\071\001\072\001\
\007\001\255\255\009\001\255\255\255\255\255\255\079\001\255\255\
\081\001\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\255\255\000\000\025\001\026\001\255\255\028\001\255\255\255\255\
\031\001\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\061\001\062\001\
\063\001\064\001\255\255\255\255\255\255\255\255\069\001\255\255\
\071\001\072\001\255\255\255\255\255\255\255\255\255\255\255\255\
\079\001\255\255\081\001\255\255\001\001\002\001\255\255\255\255\
\255\255\255\255\007\001\000\000\009\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\255\255\255\255\255\255\025\001\026\001\255\255\028\001\
\255\255\255\255\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\061\001\062\001\063\001\064\001\255\255\255\255\001\001\002\001\
\069\001\255\255\071\001\072\001\007\001\000\000\009\001\255\255\
\255\255\255\255\079\001\255\255\081\001\255\255\255\255\255\255\
\255\255\020\001\021\001\255\255\255\255\255\255\025\001\026\001\
\255\255\028\001\255\255\255\255\031\001\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\061\001\062\001\063\001\064\001\255\255\255\255\
\001\001\002\001\069\001\255\255\071\001\072\001\007\001\000\000\
\009\001\255\255\255\255\255\255\079\001\255\255\081\001\255\255\
\255\255\255\255\255\255\020\001\021\001\255\255\255\255\255\255\
\025\001\026\001\255\255\028\001\255\255\255\255\031\001\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\255\255\255\255\061\001\062\001\063\001\064\001\
\255\255\000\000\001\001\002\001\069\001\255\255\071\001\072\001\
\007\001\255\255\009\001\255\255\255\255\255\255\079\001\255\255\
\081\001\255\255\255\255\255\255\255\255\020\001\021\001\255\255\
\255\255\255\255\025\001\026\001\255\255\028\001\255\255\255\255\
\031\001\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\061\001\062\001\
\063\001\064\001\255\255\000\000\001\001\002\001\069\001\255\255\
\071\001\072\001\007\001\255\255\009\001\255\255\255\255\255\255\
\079\001\255\255\081\001\255\255\255\255\255\255\255\255\020\001\
\021\001\255\255\255\255\255\255\025\001\026\001\255\255\028\001\
\255\255\255\255\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\061\001\062\001\063\001\064\001\255\255\000\000\255\255\002\001\
\069\001\255\255\071\001\072\001\007\001\255\255\009\001\255\255\
\255\255\255\255\079\001\255\255\081\001\255\255\255\255\255\255\
\255\255\020\001\021\001\255\255\255\255\255\255\025\001\026\001\
\255\255\028\001\255\255\255\255\255\255\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\053\001\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\061\001\062\001\063\001\064\001\255\255\000\000\
\255\255\002\001\069\001\255\255\071\001\072\001\007\001\255\255\
\009\001\255\255\255\255\255\255\079\001\255\255\081\001\255\255\
\255\255\255\255\255\255\020\001\021\001\255\255\255\255\255\255\
\025\001\026\001\255\255\028\001\255\255\255\255\255\255\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\001\001\002\001\061\001\062\001\063\001\064\001\
\007\001\255\255\009\001\255\255\069\001\255\255\071\001\072\001\
\255\255\255\255\255\255\255\255\255\255\020\001\079\001\255\255\
\081\001\255\255\025\001\026\001\255\255\028\001\255\255\255\255\
\031\001\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\053\001\054\001\
\055\001\056\001\057\001\058\001\255\255\255\255\255\255\062\001\
\063\001\064\001\255\255\255\255\001\001\002\001\255\255\255\255\
\071\001\072\001\007\001\255\255\009\001\255\255\255\255\255\255\
\079\001\255\255\081\001\255\255\255\255\000\000\255\255\020\001\
\255\255\255\255\255\255\255\255\025\001\026\001\255\255\028\001\
\255\255\255\255\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\000\000\062\001\063\001\064\001\255\255\255\255\001\001\002\001\
\255\255\255\255\071\001\072\001\007\001\255\255\009\001\255\255\
\255\255\255\255\079\001\255\255\081\001\255\255\255\255\255\255\
\255\255\020\001\255\255\255\255\255\255\255\255\025\001\026\001\
\255\255\255\255\255\255\255\255\031\001\032\001\033\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\053\001\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\255\255\062\001\063\001\064\001\255\255\255\255\
\001\001\002\001\255\255\255\255\071\001\072\001\007\001\255\255\
\009\001\255\255\255\255\255\255\079\001\255\255\081\001\255\255\
\255\255\255\255\255\255\020\001\255\255\255\255\255\255\255\255\
\025\001\026\001\000\000\028\001\255\255\255\255\031\001\032\001\
\033\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\255\255\255\255\255\255\007\001\
\255\255\009\001\255\255\255\255\053\001\054\001\055\001\056\001\
\057\001\058\001\255\255\255\255\020\001\062\001\063\001\064\001\
\255\255\025\001\026\001\000\000\028\001\255\255\071\001\031\001\
\032\001\033\001\255\255\255\255\255\255\255\255\079\001\255\255\
\081\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\053\001\054\001\055\001\
\056\001\057\001\058\001\001\001\002\001\255\255\062\001\255\255\
\255\255\007\001\255\255\255\255\010\001\255\255\000\000\071\001\
\072\001\255\255\255\255\255\255\018\001\255\255\020\001\079\001\
\255\255\081\001\255\255\025\001\026\001\255\255\255\255\255\255\
\255\255\031\001\032\001\033\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\001\001\255\255\
\255\255\255\255\255\255\255\255\007\001\255\255\255\255\255\255\
\054\001\055\001\056\001\057\001\058\001\255\255\255\255\255\255\
\062\001\020\001\021\001\255\255\255\255\255\255\025\001\026\001\
\000\000\071\001\255\255\255\255\031\001\032\001\033\001\255\255\
\255\255\079\001\255\255\081\001\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\255\255\007\001\
\255\255\255\255\255\255\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\061\001\062\001\020\001\021\001\255\255\255\255\
\255\255\025\001\026\001\000\000\071\001\255\255\255\255\031\001\
\032\001\033\001\255\255\255\255\079\001\255\255\081\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\054\001\055\001\
\056\001\057\001\058\001\002\001\255\255\061\001\062\001\255\255\
\007\001\255\255\255\255\010\001\000\000\255\255\255\255\071\001\
\255\255\255\255\255\255\018\001\255\255\020\001\255\255\079\001\
\255\255\081\001\025\001\026\001\255\255\255\255\255\255\255\255\
\255\255\032\001\033\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\002\001\255\255\255\255\255\255\
\255\255\007\001\255\255\255\255\010\001\255\255\255\255\054\001\
\055\001\056\001\057\001\058\001\018\001\255\255\020\001\062\001\
\000\000\255\255\255\255\025\001\026\001\255\255\255\255\255\255\
\071\001\255\255\032\001\033\001\255\255\255\255\255\255\255\255\
\079\001\255\255\081\001\255\255\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\255\255\255\255\255\255\255\255\255\255\
\054\001\055\001\056\001\057\001\058\001\255\255\255\255\020\001\
\062\001\255\255\255\255\000\000\025\001\026\001\255\255\255\255\
\255\255\071\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\079\001\255\255\081\001\255\255\255\255\255\255\001\001\
\255\255\255\255\255\255\255\255\255\255\007\001\255\255\255\255\
\255\255\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\061\001\062\001\020\001\255\255\000\000\255\255\255\255\025\001\
\026\001\255\255\071\001\255\255\255\255\031\001\032\001\033\001\
\255\255\255\255\079\001\255\255\081\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\054\001\055\001\056\001\057\001\
\058\001\001\001\255\255\061\001\062\001\255\255\255\255\007\001\
\255\255\255\255\255\255\255\255\255\255\071\001\255\255\255\255\
\255\255\255\255\255\255\255\255\020\001\079\001\255\255\081\001\
\255\255\025\001\026\001\255\255\255\255\255\255\255\255\031\001\
\032\001\033\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\255\255\255\255\255\255\054\001\055\001\
\056\001\057\001\058\001\255\255\255\255\061\001\062\001\020\001\
\255\255\255\255\255\255\255\255\025\001\026\001\255\255\071\001\
\255\255\255\255\031\001\032\001\033\001\255\255\255\255\079\001\
\255\255\081\001\255\255\255\255\255\255\001\001\255\255\255\255\
\255\255\255\255\255\255\007\001\255\255\255\255\255\255\255\255\
\053\001\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\020\001\062\001\255\255\255\255\255\255\025\001\026\001\255\255\
\255\255\255\255\071\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\079\001\255\255\081\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\054\001\055\001\056\001\057\001\058\001\007\001\
\255\255\061\001\062\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\071\001\020\001\255\255\255\255\255\255\
\255\255\025\001\026\001\079\001\255\255\081\001\255\255\031\001\
\032\001\033\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\255\255\255\255\255\255\
\255\255\255\255\007\001\255\255\255\255\255\255\054\001\055\001\
\056\001\057\001\058\001\255\255\255\255\061\001\062\001\020\001\
\255\255\255\255\255\255\255\255\025\001\026\001\255\255\071\001\
\255\255\255\255\031\001\032\001\033\001\255\255\255\255\079\001\
\255\255\081\001\001\001\255\255\255\255\001\001\005\001\255\255\
\255\255\255\255\255\255\007\001\255\255\012\001\255\255\255\255\
\255\255\054\001\055\001\056\001\057\001\058\001\255\255\255\255\
\020\001\062\001\255\255\255\255\255\255\025\001\026\001\255\255\
\255\255\255\255\071\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\079\001\255\255\081\001\044\001\045\001\046\001\
\255\255\048\001\049\001\050\001\051\001\255\255\255\255\255\255\
\255\255\255\255\054\001\055\001\056\001\057\001\058\001\255\255\
\255\255\061\001\062\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\071\001\075\001\076\001\255\255\255\255\
\079\001\080\001\081\001\082\001\083\001\081\001\001\001\255\255\
\255\255\255\255\005\001\006\001\007\001\255\255\255\255\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\255\255\
\255\255\020\001\001\001\255\255\255\255\255\255\025\001\026\001\
\007\001\255\255\255\255\255\255\031\001\255\255\033\001\255\255\
\255\255\255\255\255\255\255\255\255\255\020\001\255\255\255\255\
\255\255\255\255\255\255\026\001\047\001\255\255\255\255\255\255\
\031\001\255\255\255\255\054\001\055\001\056\001\057\001\058\001\
\255\255\255\255\255\255\062\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\071\001\255\255\255\255\255\255\
\055\001\056\001\057\001\255\255\079\001\255\255\081\001\062\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\071\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\079\001\255\255\081\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  LT\000\
  GT\000\
  EOF\000\
  BANG\000\
  CARAT\000\
  BAR\000\
  EQUAL\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  LTGT\000\
  LTEQUAL\000\
  GTEQUAL\000\
  COMMA\000\
  DOT\000\
  AT\000\
  SEMI\000\
  COLON\000\
  UNDERSCORE\000\
  PRIME\000\
  NEW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  STRING\000\
  INT\000\
  NULL\000\
  LET\000\
  TYPE\000\
  IN\000\
  OUT\000\
  SYSTEM\000\
  ARROW\000\
  REC\000\
  BOOL\000\
  CHAR\000\
  LIST\000\
  FLOAT\000\
  SUB\000\
  EMPTYLIST\000\
  TRUE\000\
  FALSE\000\
  CONS\000\
  SHOW\000\
  INT2FLOAT\000\
  FLOAT2INT\000\
  SQRT\000\
  TAU\000\
  AND\000\
  VAL\000\
  QUESTION\000\
  DO\000\
  REPLICATE\000\
  RUN\000\
  SAMPLE\000\
  MOV\000\
  DIRECTIVE\000\
  DELAY\000\
  OR\000\
  WITH\000\
  CHAN\000\
  PROC\000\
  PLOT\000\
  OF\000\
  AS\000\
  ALL\000\
  MATCH\000\
  CASE\000\
  SPACE\000\
  SHAPE\000\
  CUBOID\000\
  SPHERE\000\
  ORIGIN\000\
  COORD\000\
  "

let yynames_block = "\
  INTVALUE\000\
  STRINGVALUE\000\
  NAME\000\
  FLOATVALUE\000\
  CHARVALUE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'directives) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 665 "parser.mly"
                         (export (_2 initial_directives),encode (global2 (check _3))   )
# 1837 "parser.ml"
               : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 666 "parser.mly"
                               ( export initial_directives,encode (global2 (check _1)) )
# 1844 "parser.ml"
               : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'directives) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 669 "parser.mly"
                         (export (_2 initial_directives),encode (check _3)   )
# 1852 "parser.ml"
               : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 670 "parser.mly"
                               ( export initial_directives,encode (check _1) )
# 1859 "parser.ml"
               : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 673 "parser.mly"
                   (fun d -> if _1 = "graph" then {d with graph=true} else
                             if _1 = "debug" then {d with debug=true} else d )
# 1867 "parser.ml"
               : 'directives))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 675 "parser.mly"
                               ( fun d -> {d with duration=_2}               )
# 1874 "parser.ml"
               : 'directives))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : float) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 676 "parser.mly"
                               ( fun d -> {d with duration=_2; resolution=_3})
# 1882 "parser.ml"
               : 'directives))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ((Action.t*string) list * (Species.t*string) list)) in
    Obj.repr(
# 677 "parser.mly"
                               ( fun d -> {d with plots = _2}                )
# 1889 "parser.ml"
               : 'directives))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'directives) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'directives) in
    Obj.repr(
# 678 "parser.mly"
                                  ( fun d -> _1 (_3 d)                       )
# 1897 "parser.ml"
               : 'directives))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : action) in
    Obj.repr(
# 681 "parser.mly"
                               ( [Action.eval _1,Action.to_string "" _1],[]  )
# 1904 "parser.ml"
               : ((Action.t*string) list * (Species.t*string) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : action) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 682 "parser.mly"
                               ( [Action.eval _1, _3],[]                     )
# 1912 "parser.ml"
               : ((Action.t*string) list * (Species.t*string) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 683 "parser.mly"
                               ( [],[(_1,_2),_4]                             )
# 1921 "parser.ml"
               : ((Action.t*string) list * (Species.t*string) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t list) in
    Obj.repr(
# 684 "parser.mly"
                               ( [],[(_1,_2),Species.to_string(_1,_2)]       )
# 1929 "parser.ml"
               : ((Action.t*string) list * (Species.t*string) list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : ((Action.t*string) list * (Species.t*string) list)) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : ((Action.t*string) list * (Species.t*string) list)) in
    Obj.repr(
# 685 "parser.mly"
                               ( (fst _1 @ fst _3),(snd _1 @ snd _3)         )
# 1937 "parser.ml"
               : ((Action.t*string) list * (Species.t*string) list)))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 689 "parser.mly"
                               ( New((pos()),[Value.Channel(_2,0,_4,Value.Float infinity,_6)],_7) )
# 1947 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : Value.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 691 "parser.mly"
                               ( New((pos()),[Value.Channel(_2,0,_4,_6,_8)],_9) )
# 1958 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 693 "parser.mly"
                               ( New((pos()),[Value.Channel(_2,0,Value.Float infinity,Value.Float infinity,_4)],_5) )
# 1967 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 694 "parser.mly"
                               ( Type(_2,_4,_5)                              )
# 1976 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Pattern.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 695 "parser.mly"
                               ( Value((pos()),_2,_4,_5)                     )
# 1985 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 696 "parser.mly"
                               ( Definitions(_2,_3)                          )
# 1993 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 697 "parser.mly"
                               ( Parallel([_2;_3])                           )
# 2001 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 698 "parser.mly"
                               ( _2                                          )
# 2008 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 699 "parser.mly"
                               ( _1                                          )
# 2015 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    Obj.repr(
# 700 "parser.mly"
                               ( Null                                        )
# 2021 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'd2) in
    Obj.repr(
# 703 "parser.mly"
                               ( New((pos()),[Value.Channel(_2,0,_4,Value.Float infinity,_6)],_7) )
# 2031 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'd2) in
    Obj.repr(
# 704 "parser.mly"
                               ( New((pos()),[Value.Channel(_2,0,Value.Float infinity,Value.Float infinity,_4)],_5) )
# 2040 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'd2) in
    Obj.repr(
# 705 "parser.mly"
                               ( Type(_2,_4,_5)                              )
# 2049 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Pattern.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'd2) in
    Obj.repr(
# 706 "parser.mly"
                               ( Value((pos()),_2,_4,_5)                     )
# 2058 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'd2) in
    Obj.repr(
# 707 "parser.mly"
                               ( Parallel([_2;_3])                           )
# 2066 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 708 "parser.mly"
                               ( _2                                          )
# 2073 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 709 "parser.mly"
                               ( _1                                          )
# 2080 "parser.ml"
               : 'd2))
; (fun __caml_parser_env ->
    Obj.repr(
# 712 "parser.mly"
                               ( Null                                        )
# 2086 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : position*action*t) in
    Obj.repr(
# 713 "parser.mly"
                               ( Summation([_1],false)                       )
# 2093 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ps) in
    Obj.repr(
# 714 "parser.mly"
                               ( Parallel(_2)                                )
# 2100 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t list) in
    Obj.repr(
# 715 "parser.mly"
                               ( Instance((pos()),_1,_2)                     )
# 2108 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 716 "parser.mly"
                               ( Parallel [Instance((pos()),_1,_2);_4]       )
# 2117 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : position*action*t) in
    Obj.repr(
# 717 "parser.mly"
                               ( Replicate(_2)                               )
# 2124 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : (position*action*t) list) in
    Obj.repr(
# 718 "parser.mly"
                               ( Summation(_2,false)                         )
# 2131 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : (position*action*t) list) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    Obj.repr(
# 719 "parser.mly"
                               ( Summation(_2,true)                          )
# 2139 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 720 "parser.mly"
                               ( Match(_2,_4)                                )
# 2147 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Value.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : t) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 721 "parser.mly"
                               ( Match(_2,[(pos()),Value.Bool(true),_4;(pos()),Value.Bool(false),_6])   )
# 2156 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 722 "parser.mly"
                               ( Match(_2,[(pos()),Value.Bool(true),_4;(pos()),Value.Bool(false),Null]) )
# 2164 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 723 "parser.mly"
                               ( Repeat(_1,_3)                               )
# 2172 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'd2) in
    Obj.repr(
# 724 "parser.mly"
                               ( _2                                          )
# 2179 "parser.ml"
               : t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 8 : Pattern.t list) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : Value.t) in
    let _6 = (Parsing.peek_val __caml_parser_env 4 : Value.t) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'shape) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 728 "parser.mly"
                               ( [(pos()),_1,_2,_4,_6,_8,_10]                )
# 2191 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'def) in
    Obj.repr(
# 729 "parser.mly"
                               ( _1 @ _3                                     )
# 2199 "parser.ml"
               : 'def))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 732 "parser.mly"
                               ( [_1;_3]                                     )
# 2207 "parser.ml"
               : 'ps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ps) in
    Obj.repr(
# 733 "parser.mly"
                               ( _1 :: _3                                    )
# 2215 "parser.ml"
               : 'ps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : position*action*t) in
    Obj.repr(
# 736 "parser.mly"
                               ( [_1]                                     )
# 2222 "parser.ml"
               : (position*action*t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : (position*action*t) list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : position*action*t) in
    Obj.repr(
# 737 "parser.mly"
                               ( _3 :: _1                                    )
# 2230 "parser.ml"
               : (position*action*t) list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 740 "parser.mly"
                               ( [(pos()),_1,_3]                             )
# 2238 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : t) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'cases) in
    Obj.repr(
# 741 "parser.mly"
                               ( ((pos()),_1,_3) :: _5                       )
# 2247 "parser.ml"
               : 'cases))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : action) in
    Obj.repr(
# 745 "parser.mly"
                               ( ((pos()),_1,Null)                           )
# 2254 "parser.ml"
               : position*action*t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : action) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : t) in
    Obj.repr(
# 746 "parser.mly"
                               ( ((pos()),_1,_3)                             )
# 2262 "parser.ml"
               : position*action*t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Pattern.t list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 749 "parser.mly"
                               ( Action.Input(_2,_3,_5)                      )
# 2271 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Pattern.t list) in
    Obj.repr(
# 750 "parser.mly"
                               ( Action.Input(_2,_3,Value.Float 1.0)         )
# 2279 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 751 "parser.mly"
                               ( Action.Input(_2,[],_4)                      )
# 2287 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 752 "parser.mly"
                               ( Action.Input(_2,[],Value.Float 1.0)         )
# 2294 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Value.t list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 753 "parser.mly"
                               ( Action.Output(_2,_3,_5)                     )
# 2303 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t list) in
    Obj.repr(
# 754 "parser.mly"
                               ( Action.Output(_2,_3,Value.Float 1.0)        )
# 2311 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 755 "parser.mly"
                               ( Action.Output(_2,[],_4)                     )
# 2319 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 756 "parser.mly"
                               ( Action.Output(_2,[],Value.Float 1.0)        )
# 2326 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 757 "parser.mly"
                               ( Action.Delay(_3)                            )
# 2333 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 758 "parser.mly"
                               ( Action.Delay(_2)                            )
# 2340 "parser.ml"
               : action))
; (fun __caml_parser_env ->
    Obj.repr(
# 762 "parser.mly"
                               ( Pattern.Wild                                )
# 2346 "parser.ml"
               : Pattern.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Typ.t) in
    Obj.repr(
# 763 "parser.mly"
                               ( Pattern.Typed(_1,_3)                        )
# 2354 "parser.ml"
               : Pattern.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 764 "parser.mly"
                               ( Pattern.Name(_1)                            )
# 2361 "parser.ml"
               : Pattern.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'm2) in
    Obj.repr(
# 765 "parser.mly"
                               ( Pattern.Tuple(_2)                           )
# 2368 "parser.ml"
               : Pattern.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Pattern.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Pattern.t) in
    Obj.repr(
# 768 "parser.mly"
                               ( [_1;_3]                                     )
# 2376 "parser.ml"
               : 'm2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Pattern.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'm2) in
    Obj.repr(
# 769 "parser.mly"
                               ( _1 :: _3                                    )
# 2384 "parser.ml"
               : 'm2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'm2) in
    Obj.repr(
# 772 "parser.mly"
                               ( check_duplicates _2 (pos())                 )
# 2391 "parser.ml"
               : Pattern.t list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Pattern.t) in
    Obj.repr(
# 773 "parser.mly"
                               ( check_duplicates [_2] (pos())               )
# 2398 "parser.ml"
               : Pattern.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 774 "parser.mly"
                               ( []                                          )
# 2404 "parser.ml"
               : Pattern.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 777 "parser.mly"
                               ( Value.Name(_1)                              )
# 2411 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 780 "parser.mly"
                               ( Value.String(_1)                            )
# 2418 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 781 "parser.mly"
                               ( Value.Int(_1)                               )
# 2425 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 782 "parser.mly"
                               ( Value.Bool(true)                            )
# 2431 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 783 "parser.mly"
                               ( Value.Bool(false)                           )
# 2437 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 784 "parser.mly"
                               ( Value.Char(_1)                              )
# 2444 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 785 "parser.mly"
                               ( Value.Float(_1)                             )
# 2451 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 786 "parser.mly"
                               ( Value.Name(_1)                              )
# 2458 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 787 "parser.mly"
                               ( Value.Op(_1,Value.Plus,_3)                  )
# 2466 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 788 "parser.mly"
                               ( Value.Op(_1,Value.Minus,_3)                 )
# 2474 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 789 "parser.mly"
                               ( Value.Op(_1,Value.Mul,_3)                   )
# 2482 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 790 "parser.mly"
                               ( Value.Op(_1,Value.Div,_3)                   )
# 2490 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 791 "parser.mly"
                               ( Value.Op(_1,Value.Equal,_3)                 )
# 2498 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 792 "parser.mly"
                               ( Value.Op(_1,Value.Lt,_3)                    )
# 2506 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 793 "parser.mly"
                               ( Value.Op(_1,Value.Gt,_3)                    )
# 2514 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 794 "parser.mly"
                               ( Value.Op(_1,Value.Different,_3)             )
# 2522 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 795 "parser.mly"
                               ( Value.Op(_1,Value.Ltequal,_3)               )
# 2530 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 796 "parser.mly"
                               ( Value.Op(_1,Value.Gtequal,_3)               )
# 2538 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 797 "parser.mly"
                               ( Value.Neg(_2)                               )
# 2545 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 798 "parser.mly"
                               ( Value.Show(_2)                              )
# 2552 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 799 "parser.mly"
                               ( Value.List([])                              )
# 2558 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 800 "parser.mly"
                               ( Value.Cons(_1,_3)                           )
# 2566 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t list) in
    Obj.repr(
# 801 "parser.mly"
                               ( Value.Data(_1,_2)                           )
# 2574 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 802 "parser.mly"
                               ( Value.Function(Value.Int2Float,_2)          )
# 2581 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 803 "parser.mly"
                               ( Value.Function(Value.Float2Int,_2)          )
# 2588 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 804 "parser.mly"
                               ( Value.Function(Value.Sqrt,_2)               )
# 2595 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'v2) in
    Obj.repr(
# 805 "parser.mly"
                               ( Value.Tuple(_2)                             )
# 2602 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    Obj.repr(
# 806 "parser.mly"
                               ( _2                                          )
# 2609 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'shape) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 807 "parser.mly"
                               ( Value.Space(_1,_3)                          )
# 2617 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'loc) in
    Obj.repr(
# 808 "parser.mly"
                               ( _1                                          )
# 2624 "parser.ml"
               : Value.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Value.t) in
    Obj.repr(
# 811 "parser.mly"
                               ( [_1; _3]                                    )
# 2632 "parser.ml"
               : 'v2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Value.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'v2) in
    Obj.repr(
# 812 "parser.mly"
                               ( _1::_3                                      )
# 2640 "parser.ml"
               : 'v2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'v2) in
    Obj.repr(
# 815 "parser.mly"
                               ( (_2)                                        )
# 2647 "parser.ml"
               : Value.t list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Value.t) in
    Obj.repr(
# 816 "parser.mly"
                               ( [_2]                                        )
# 2654 "parser.ml"
               : Value.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 817 "parser.mly"
                               ( []                                          )
# 2660 "parser.ml"
               : Value.t list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 821 "parser.mly"
                               ( Typ.Poly(_2)                                )
# 2667 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 822 "parser.mly"
                               ( Typ.String                                  )
# 2673 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 823 "parser.mly"
                               ( Typ.Int                                     )
# 2679 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 824 "parser.mly"
                               ( Typ.Char                                    )
# 2685 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 825 "parser.mly"
                               ( Typ.Float                                   )
# 2691 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 826 "parser.mly"
                               ( Typ.Bool                                    )
# 2697 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 827 "parser.mly"
                               ( Typ.Name(_1)                                )
# 2704 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 828 "parser.mly"
                               ( Typ.Channel([])                             )
# 2710 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Typ.t list) in
    Obj.repr(
# 829 "parser.mly"
                               ( Typ.Channel(_2)                             )
# 2717 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Typ.t list) in
    Obj.repr(
# 830 "parser.mly"
                               ( Typ.Process(_2)                             )
# 2724 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    Obj.repr(
# 831 "parser.mly"
                               ( Typ.List(_3)                                )
# 2731 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'data) in
    Obj.repr(
# 832 "parser.mly"
                               ( Typ.Data(_1)                                )
# 2738 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't2) in
    Obj.repr(
# 833 "parser.mly"
                               ( Typ.Tuple(_2)                               )
# 2745 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 834 "parser.mly"
                               ( Typ.Shape                                   )
# 2751 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 835 "parser.mly"
                               ( Typ.Space                                   )
# 2757 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 836 "parser.mly"
                               ( Typ.Coordinate                              )
# 2763 "parser.ml"
               : Typ.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Typ.t list) in
    Obj.repr(
# 839 "parser.mly"
                               ( [_1,_2]                                     )
# 2771 "parser.ml"
               : 'data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'data) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'data) in
    Obj.repr(
# 840 "parser.mly"
                               ( _1 @ _3                                     )
# 2779 "parser.ml"
               : 'data))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Typ.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Typ.t) in
    Obj.repr(
# 843 "parser.mly"
                               ( [_1;_3]                                     )
# 2787 "parser.ml"
               : 't2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Typ.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 't2) in
    Obj.repr(
# 844 "parser.mly"
                               ( _1 :: _3                                    )
# 2795 "parser.ml"
               : 't2))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 't2) in
    Obj.repr(
# 847 "parser.mly"
                               ( _2                                          )
# 2802 "parser.ml"
               : Typ.t list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Typ.t) in
    Obj.repr(
# 848 "parser.mly"
                               ( [_2]                                        )
# 2809 "parser.ml"
               : Typ.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 849 "parser.mly"
                               ( []                                          )
# 2815 "parser.ml"
               : Typ.t list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : float) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 856 "parser.mly"
                               ( Value.Coordinate(_2,_4,_6)                 )
# 2824 "parser.ml"
               : 'loc))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 859 "parser.mly"
                               ( Shape.Sphere(_3)                           )
# 2831 "parser.ml"
               : 'shape))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : float) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : float) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 861 "parser.mly"
                               ( Shape.Cuboid(_3, _5, _7)                   )
# 2840 "parser.ml"
               : 'shape))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry main2 *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry plots *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry v *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry vs *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry m *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry ms *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry x *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry t *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string))
let main2 (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string))
let plots (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 3 lexfun lexbuf : ((Action.t*string) list * (Species.t*string) list))
let v (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 4 lexfun lexbuf : Value.t)
let vs (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 5 lexfun lexbuf : Value.t list)
let m (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 6 lexfun lexbuf : Pattern.t)
let ms (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 7 lexfun lexbuf : Pattern.t list)
let x (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 8 lexfun lexbuf : Value.t)
let t (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 9 lexfun lexbuf : Typ.t)
