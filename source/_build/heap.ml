(*
 * Decide on garbage-collection
 * Plot speciess based on the source names of the arguments, rather than an exact match.
*)

(*****************************************************************************)
type substore = Substore.t
type choice = Choice.t
type species = Species.t
type point = species*int
type environment = Environment.t
type value = Value.t
type name = string
type substitution = (name*value) list

(*IF-OCAML*)
module Map = Map.Make(Species)
type t = (int * substore * choice) Map.t

let find (i:species) (h:t) =
  try Some(Map.find i h)
  with Not_found -> None
(*ENDIF-OCAML*)

(*F#
let Map = Map.Make(Species.compare)
type t = Tagged.Map<Species.t, (int * substore * choice)>
let find (i:species) (h:t) = Map.tryfind i h
F#*)

let fold (f:(species -> 'a -> 'a)) (x:'a) (h:t) =
  let f' (i:species) (n,s,c) (acc:'a) =
    let rec f'' (n:int) (acc:'a) =
      if n = 0 then acc
      else f'' (n - 1) (f i acc)
    in f'' n acc
  in Map.fold f' h x

let empty = Map.empty

let display (html:bool) (h:t) =
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let arrow = if html then symbol " -&gt; " else " -> " in
  let newline = if html then "<br>" else "\n" in
  let f (i:species) (n,s,c) (acc:string) =
    newline ^ Species.to_string i ^ symbol arrow ^
    string_of_int n ^ symbol " * " ^ Choice.to_string c ^ Substore.display html s ^ acc
  in Map.fold f h ""

let to_string (context:string) (h:t) =
  let f (i:species) (n,s,c) (acc:string) =
    context ^ ".heap.species," ^ Species.to_string i ^ "\n" ^
    context ^ ".heap.choice," ^ string_of_int n ^ " * " ^ Choice.to_string c ^
    "\n" ^
    Substore.to_string (context ^ ".heap.substore") s ^ "\n" ^ acc
  in Map.fold f h ""
let to_html (h:t) = display true h

let add (i:species) (n,s,c) (h:t) =
 if n=0 then Map.remove i h
 else Map.add i (n,s,c) h

let remove (i:species) (h:t) = match find i h with
    None -> None
  | Some(counter,s,c) ->
      if counter <= 0
      then None
      else Some(add i (counter-1,s,c) h,s,c)

let plot (speciess:species list) (h:t) =
  let initialise (speciess:species list) =
    let f (i:species) = (i,0)
    in List.map f speciess
  in
  let add (i:species) (n:int) (points:point list) =
    let f (i',n') = if Species.matches i' i then (i',n'+n) else (i',n')
    in List.map f points
  in
  let update (points:point list) (h:t) =
    let f (i:species) ((counter:int),(s:substore),(c:choice)) (points:point list) =
      add (Species.source i) counter points
    in Map.fold f h points
  in
  let extract (points:point list) = List.map snd points
  in extract (update (initialise speciess) h)
(*****************************************************************************)

