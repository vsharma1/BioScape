type store = Store.t
type substore = Substore.t
type action = Store.action
type record = Store.record
type choice = Choice.t
type proc = Choice.proc
type value = Choice.value
type heap = Heap.t
type volume = Volume.t
type environment = Environment.t
type pattern = Environment.pattern
type definition = Environment.definition
type space = Value.t
type shape = Shape.t
type species = Species.t
type process = Process.t
type pos = Utils.pos

type t = {
  environment: environment;
  counter: int;
  heap: heap;
  store: store;
  volume: volume;
  time: float
}
let get_environment (t:t) = t.environment
let get_heap (t:t) = t.heap
let get_volume (t:t) = t.volume

let empty = {
  environment = Environment.empty;
  counter = 0;
  heap = Heap.empty;
  store = Store.empty;
  volume = Volume.empty;
  time = 0.
}

let init (e:environment) (v:volume) =
  {
    empty with
    environment=e;
    volume=v;
  }

let init_volume (t:t) =
  let v = Heap.fold (fun x acc -> Volume.add x acc) t.volume t.heap in
  {t with volume = v}

let display (html:bool) (t:t) =
  let newline = if html then "<br>" else "\n" in
    newline ^ "*Counter: " ^ string_of_int t.counter ^
    newline ^ "*Time: " ^ string_of_float t.time ^
    newline ^ "*Volume:" ^ newline ^ Volume.display html t.volume ^
    newline ^ newline ^ "*Environment:" ^ newline ^ Environment.display html t.environment ^
    newline ^ newline ^ "*Store:" ^ newline ^ Store.display html t.store ^
    newline ^ newline ^ "*Heap:" ^ newline ^ Heap.display html t.heap

let to_string (t:t) =
  Store.to_string "term" t.store ^ "\n" ^
  Heap.to_string "term" t.heap

let to_html (t:t) = display true t

let fresh (t:t) =
  let t = {t with counter = t.counter+1}
  in t.counter,t

let remove_delay (x:value) (index:float) (t:t) =
  match Store.lookup_delay x index t.store with
  | index,None -> None
  | index,Some(i) ->
      match Heap.remove i t.heap with
      | None -> None
      | Some(h,s,c) ->
          match Choice.find_action index (Action.Delay(x)) c with
          | None -> None
          | Some(index,v,k,p) -> Some(i,v,k,p,
          {t with
          heap = h;
          store = Store.minus t.time i s t.store})

let remove_output (x:value) (index:float) (t:t) =
  match Store.lookup_output x index t.store with
  | index,None ->  None
  | index,Some(i) ->
      match Heap.remove i t.heap with
      | None ->  None
      | Some(h,s,c) ->
          match Choice.find_action index (Action.Output(x,[],Value.empty)) c with
          | None -> None
          | Some(index,v,k,p) -> Some(i,v,k,p,{t with heap = h; store = Store.minus t.time i s t.store})

let remove_input (x:value) (index:float) (t:t) =
  match Store.lookup_input x index t.store with
  | index,None -> None
  | index,Some(external_outputs,i) ->
      match Heap.remove i t.heap with
      | None ->  None
      | Some(h,s,c) ->
          match Choice.find_action (index/.external_outputs) (Action.Input(x,[],Value.empty)) c with
          | None -> None
          | Some(index',v,k,p) ->
              let output_index = index/.index'
              in Some(i,v,k,p,output_index,{t with heap = h; store = Store.minus t.time i s t.store})

let add (i:species) (c:choice) (t:t) =
  match Heap.find i t.heap with
  | Some(counter,s,c) -> {
    t with
    heap   = Heap.add i (counter+1,s,c) t.heap;
    store  = Store.plus t.time i s t.store}
  | None ->
      let vol,s = Choice.create_substore t.volume c
      in {
        t with
        volume = vol;
        heap  = Heap.add i (1,s,c) t.heap;
        store = Store.plus t.time i s t.store}

let find (n:value) (t:t) = Environment.find n t.environment
let gillespie (t:t) = Store.gillespie t.store
let flip (time:float) (v:value) (t:t) = {t with store = Store.flip v t.store; time = time}
let plot (ks:action list) (is:species list) (t:t) = Store.plot ks t.store @ Heap.plot is t.heap
let plot_vol (is:species list) (t:t) = Volume.plot is t.volume
let debug (time:float) (t:t) = string_of_float time ^ "\t" ^ Store.to_string "" t.store ^ "\n"

let react (ch:string) (radius:float) (s:species list) (p:process list) (delay:float) (t:t) =
  let products = List.fold_right (fun x acc -> (Process.to_species x) @ acc) p [] in
  let success, vol = Volume.react ch radius s products delay t.volume in
  success, {t with volume = vol}
(*****************************************************************************)

