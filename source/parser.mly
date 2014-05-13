/* File parser.mly */
/* ocamlyacc parser.mly */

%{
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

%}

%token LPAREN RPAREN LBRACKET RBRACKET LT GT
%token EOF BANG CARAT BAR
%token EQUAL PLUS MINUS STAR SLASH LTGT LTEQUAL GTEQUAL
%token COMMA DOT AT SEMI COLON UNDERSCORE PRIME
%token NEW IF THEN ELSE STRING INT NULL LET TYPE IN OUT SYSTEM ARROW REC
%token BOOL CHAR LIST FLOAT SUB EMPTYLIST TRUE FALSE CONS SHOW INT2FLOAT FLOAT2INT SQRT
%token TAU AND VAL QUESTION DO REPLICATE RUN SAMPLE
%token MOV DIRECTIVE DELAY OR WITH CHAN PROC PLOT OF AS ALL MATCH CASE

/** BS:space tokens { **/
%token SPACE SHAPE CUBOID SPHERE ORIGIN COORD
/** } **/


%token <int> INTVALUE
%token <string> STRINGVALUE NAME
%token <float> FLOATVALUE
%token <char> CHARVALUE

%right ARROW // lowest precedence
%left BAR
%right RUN
%left COMMA
%left NEW
%left TAU
%left LET
%right SEMI
%left IN
%left AND
%left REC
%left IF
%left THEN
%left ELSE
%left DO
%left OR
%left WITH
%left BANG
%left QUESTION
%left AT
%left REPLICATE
%left VAL
%left TYPE
%left DELAY
%left MOV
%left MATCH
%left CASE

%left COLON
%left NAME SUB LIST
%left DOT
%left FLOATVALUE
%left INTVALUE

%left PLUS
%left MINUS
%left STAR
%left EQUAL
%left LTGT
%left LTEQUAL
%left GTEQUAL
%left LT
%left GT
%left SHOW
%left FLOAT2INT
%left INT2FLOAT
%left SQRT
%right CONS
%left SLASH

%left AS
%left CHAN
%left LPAREN
%left RPAREN
%left NULL
%left OF
%left STRINGVALUE
%left DIRECTIVE
%left SAMPLE

%start main // the entry point
%start main2
%start plots
%start v
%start vs
%start m
%start ms
%start x
%start t
//%start p //NB
//%start d //NB
%type <Value.t> v
%type <Value.t list> vs
%type <Pattern.t> m
%type <Pattern.t list> ms
%type <Typ.t> t
%type <Typ.t list> ts
%type <((Action.t*string) list * (Species.t*string) list)> plots
%type <(float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)> main
%type <(float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)> main2
%type <Value.t> x
%type <t> p
%type <t> d
%type <position*action*t> action
%type <(position*action*t) list> sum
%type <action> k
%%

/*****************************************************************************/
main2: // N.B need to fix encode to call global2 instead of global!
| DIRECTIVE directives d {export ($2 initial_directives),encode (global2 (check $3))   }// directive Directives Declaration
| d                            { export initial_directives,encode (global2 (check $1)) }// Declaration

main:
| DIRECTIVE directives d {export ($2 initial_directives),encode (check $3)   }// directive Directives Declaration
| d                            { export initial_directives,encode (check $1) }// Declaration

directives:                                                                   // Directives::=
| NAME             {fun d -> if $1 = "graph" then {d with graph=true} else
                             if $1 = "debug" then {d with debug=true} else d }// Name
| SAMPLE FLOATVALUE            { fun d -> {d with duration=$2}               }// sample Float
| SAMPLE FLOATVALUE INTVALUE   { fun d -> {d with duration=$2; resolution=$3}}// sample Float Integer
| PLOT plots                   { fun d -> {d with plots = $2}                }// plot Points
| directives DIRECTIVE directives { fun d -> $1 ($3 d)                       }// Directives directive Directives

plots:                                                                        // Plots::=
| k                            { [Action.eval $1,Action.to_string "" $1],[]  }// Action
| k AS STRINGVALUE             { [Action.eval $1, $3],[]                     }// Action as String
| x vs AS STRINGVALUE          { [],[($1,$2),$4]                             }// Name(Value,...,Value) as String
| x vs                         { [],[($1,$2),Species.to_string($1,$2)]       }// Name(Value,...,Value)
| plots SEMI plots             { (fst $1 @ fst $3),(snd $1 @ snd $3)         }// Plots; Plots

/*****************************************************************************/
d:                                                                            // Declarations::=
| NEW NAME AT v COLON t d      { New((pos()),[Value.Channel($2,0,$4,Value.Float infinity,$6)],$7) }// new Name@Rate:Type Declarations
| NEW NAME AT v COMMA v COLON t d
                               { New((pos()),[Value.Channel($2,0,$4,$6,$8)],$9) }// new Name(Radius)@Rate:Type Declarations
//| NEW LPAREN NAME AT v COLON t RPAREN d      { New((pos()),[Value.Channel($3,0,$5,$7)],$9) }// new (Name@Rate:Type) Declarations
| NEW NAME COLON t d           { New((pos()),[Value.Channel($2,0,Value.Float infinity,Value.Float infinity,$4)],$5) }// new Name:Type Declarations
| TYPE NAME EQUAL t d          { Type($2,$4,$5)                              }// type Name=Type Declarations
| VAL m EQUAL v d              { Value((pos()),$2,$4,$5)                     }// val Pattern=Value Declarations
| LET def d                    { Definitions($2,$3)                          }// let Definitions Declarations
| RUN p d                      { Parallel([$2;$3])                           }// run Process Declarations
| RUN p                        { $2                                          }// run Process
| p                            { $1                                          }// Process
| EOF                          { Null                                        }// EOF

d2:                                                                           // Declarations::= (without definitions)
| NEW NAME AT v COLON t d2     { New((pos()),[Value.Channel($2,0,$4,Value.Float infinity,$6)],$7) }// new Name@Rate:Type Declarations
| NEW NAME COLON t d2          { New((pos()),[Value.Channel($2,0,Value.Float infinity,Value.Float infinity,$4)],$5) }// new Name:Type Declarations
| TYPE NAME EQUAL t d2         { Type($2,$4,$5)                              }// type Name=Type Declarations
| VAL m EQUAL v d2             { Value((pos()),$2,$4,$5)                     }// val Pattern=Value Declarations
| RUN p d2                     { Parallel([$2;$3])                           }// run Process Declarations
| RUN p                        { $2                                          }// run Process
| p                            { $1                                          }// Process

p:                                                                            // Process::=
| NULL                         { Null                                        }// ()
| action                       { Summation([$1],false)                       }
| LPAREN ps RPAREN             { Parallel($2)                                }// (Processes)
| x vs                         { Instance((pos()),$1,$2)                     }// Name(Value,...,Value)
| x vs SEMI p                  { Parallel [Instance((pos()),$1,$2);$4]       }// Name(Value,...,Value) ; Process
| REPLICATE action             { Replicate($2)                               }// replicate ActionProcess
| DO sum                       { Summation($2,false)                         }// do Summation
| DO sum WITH MOV DOT x NULL   { Summation($2,true)                          }// do Summation
| MATCH v CASE cases           { Match($2,$4)                                }// match Value Cases
| IF v THEN p ELSE p           { Match($2,[(pos()),Value.Bool(true),$4;(pos()),Value.Bool(false),$6])   }// if Value then Process else Process
| IF v THEN p                  { Match($2,[(pos()),Value.Bool(true),$4;(pos()),Value.Bool(false),Null]) }// if Value then Process
| INTVALUE OF p                { Repeat($1,$3)                               }// Integer of Process
| LPAREN d2 RPAREN             { $2                                          }// (Declarations)

def:                                                                          // Definitions::=
| NAME ms AT v COMMA v COMMA shape EQUAL p
                               { [(pos()),$1,$2,$4,$6,$8,$10]                }// Name Patterns = Process
| def AND def                  { $1 @ $3                                     }// Definitions and Definitions

ps:
| p BAR p                      { [$1;$3]                                     }// Process | Process
| p BAR ps                     { $1 :: $3                                    }// Process | Processes

sum:                                                                          // Summation::=
| action                       { [$1]                                     }// ActionProcess or ActionProcess
| sum OR action                { $3 :: $1                                    }// Summation or ActionProcess

cases:                                                                        // Cases::=
| v ARROW p                    { [(pos()),$1,$3]                             }// case Value -> Process
| v ARROW p CASE cases         { ((pos()),$1,$3) :: $5                       }// Cases Cases

/*****************************************************************************/
action:                                                                       // ActionProcess::=
| k                            { ((pos()),$1,Null)                           }// Action
| k SEMI p                     { ((pos()),$1,$3)                             }// Action; Process

k:                                                                            // Action ::=
| QUESTION x ms STAR v         { Action.Input($2,$3,$5)                      }// ?Channel(Pattern,...,Pattern)*Value
| QUESTION x ms                { Action.Input($2,$3,Value.Float 1.0)         }// ?Channel(Pattern,...,Pattern)
| QUESTION x STAR v            { Action.Input($2,[],$4)                      }// ?Channel*Value
| QUESTION x                   { Action.Input($2,[],Value.Float 1.0)         }// ?Channel
| BANG x vs STAR v             { Action.Output($2,$3,$5)                     }// !Channel(Value,...,Value)*Value
| BANG x vs                    { Action.Output($2,$3,Value.Float 1.0)        }// !Channel(Value,...,Value)
| BANG x STAR v                { Action.Output($2,[],$4)                     }// !Channel*Value
| BANG x                       { Action.Output($2,[],Value.Float 1.0)        }// !Channel
| DELAY AT v                   { Action.Delay($3)                            }// delay@Value
| AT v                         { Action.Delay($2)                            }// @Value

/*****************************************************************************/
m:                                                                            // Pattern::=
| UNDERSCORE                   { Pattern.Wild                                }// _
| NAME COLON t                 { Pattern.Typed($1,$3)                        }// Name:Type
| NAME                         { Pattern.Name($1)                            }// Name
| LPAREN m2 RPAREN             { Pattern.Tuple($2)                           }// Pattern,Pattern

m2:                                                                           // Pattern2::=
| m COMMA m                    { [$1;$3]                                     }// Pattern,Pattern
| m COMMA m2                   { $1 :: $3                                    }// Pattern,Pattern2

ms:                                                                           // Patterns::=
| LPAREN m2 RPAREN             { check_duplicates $2 (pos())                 }// (Pattern,Pattern)
| LPAREN m RPAREN              { check_duplicates [$2] (pos())               }// (Pattern)
| NULL                         { []                                          }// ()

/*****************************************************************************/
x: NAME                        { Value.Name($1)                              }// Channel

v:                                                                            // Value::=
| STRINGVALUE                  { Value.String($1)                            }// String
| INTVALUE                     { Value.Int($1)                               }// Integer
| TRUE                         { Value.Bool(true)                            }// true
| FALSE                        { Value.Bool(false)                           }// false
| CHARVALUE                    { Value.Char($1)                              }// Char
| FLOATVALUE                   { Value.Float($1)                             }// Float
| NAME                         { Value.Name($1)                              }// Name
| v PLUS v                     { Value.Op($1,Value.Plus,$3)                  }// Value + Value
| v MINUS v                    { Value.Op($1,Value.Minus,$3)                 }// Value - Value
| v STAR v                     { Value.Op($1,Value.Mul,$3)                   }// Value * Value
| v SLASH v                    { Value.Op($1,Value.Div,$3)                   }// Value / Value
| v EQUAL v                    { Value.Op($1,Value.Equal,$3)                 }// Value = Value
| v LT v                       { Value.Op($1,Value.Lt,$3)                    }// Value < Value
| v GT v                       { Value.Op($1,Value.Gt,$3)                    }// Value > Value
| v LTGT v                     { Value.Op($1,Value.Different,$3)             }// Value <> Value
| v LTEQUAL v                  { Value.Op($1,Value.Ltequal,$3)               }// Value <= Value
| v GTEQUAL v                  { Value.Op($1,Value.Gtequal,$3)               }// Value >= Value
| MINUS v                      { Value.Neg($2)                               }// -Value
| SHOW v                       { Value.Show($2)                              }// show Value
| EMPTYLIST                    { Value.List([])                              }// []
| v CONS v                     { Value.Cons($1,$3)                           }// Value::Value
| NAME vs                      { Value.Data($1,$2)                           }// Name(Value,...,Value)
| INT2FLOAT v                  { Value.Function(Value.Int2Float,$2)          }// Name Value
| FLOAT2INT v                  { Value.Function(Value.Float2Int,$2)          }// Name Value
| SQRT v                       { Value.Function(Value.Sqrt,$2)               }// Name Value
| LPAREN v2 RPAREN             { Value.Tuple($2)                             }// Value,Value
| LPAREN v RPAREN              { $2                                          }// (Value)
| shape AT v                   { Value.Space($1,$3)                          }// space Name=sphere(1.0)@(1.0)
| loc                          { $1                                          }// loc

v2:                                                                           // Value2::=
| v COMMA v                    { [$1; $3]                                    }// Value,Value
| v COMMA v2                   { $1::$3                                      }// Value,Value2

vs:                                                                           // Values::=
| LPAREN v2 RPAREN             { ($2)                                        }// (Value,Value)
| LPAREN v RPAREN              { [$2]                                        }// (Value)
| NULL                         { []                                          }// ()

/*****************************************************************************/
t:                                                                            // Type::=
| PRIME NAME                   { Typ.Poly($2)                                }// 'Name
| STRING                       { Typ.String                                  }// string
| INT                          { Typ.Int                                     }// int
| CHAR                         { Typ.Char                                    }// char
| FLOAT                        { Typ.Float                                   }// float
| BOOL                         { Typ.Bool                                    }// bool
| NAME                         { Typ.Name($1)                                }// Name
| CHAN                         { Typ.Channel([])                             }// chan
| CHAN ts                      { Typ.Channel($2)                             }// chan(Type,...,Type)
| PROC ts                      { Typ.Process($2)                             }// proc(Type,...,Type)
| LIST LPAREN t RPAREN         { Typ.List($3)                                }// list(Type)
| data                         { Typ.Data($1)                                }// Data
| LPAREN t2 RPAREN             { Typ.Tuple($2)                               }// Type,Type
| SHAPE                        { Typ.Shape                                   }// Shape
| SPACE                        { Typ.Space                                   }// Space
| COORD                        { Typ.Coordinate                              }// Coordinate

data:                                                                         // Data ::=
| NAME ts                      { [$1,$2]                                     }// Name(Type,...,Type)
| data BAR data                { $1 @ $3                                     }// Data | Data

t2:                                                                           // Type2::=
| t COMMA t                    { [$1;$3]                                     }// Type,Type
| t COMMA t2                   { $1 :: $3                                    }// Type,Type2

ts:                                                                           // Types::=
| LPAREN t2 RPAREN             { $2                                          }// (Type,Type)
| LPAREN t RPAREN              { [$2]                                        }// (Type)
| NULL                         { []                                          }// ()

/*****************************************************************************/

/******************************* BS: { ***************************************/
loc:
| LT FLOATVALUE COMMA FLOATVALUE COMMA FLOATVALUE GT
                               { Value.Coordinate($2,$4,$6)                 }// Coordinate
shape:
| SPHERE LPAREN FLOATVALUE RPAREN
                               { Shape.Sphere($3)                           } // sphere(radius)
| CUBOID LPAREN FLOATVALUE COMMA FLOATVALUE COMMA FLOATVALUE RPAREN
                               { Shape.Cuboid($3, $5, $7)                   } // cuboid(width,height,depth)
/*********************************** } ***************************************/
