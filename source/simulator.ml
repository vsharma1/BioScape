type typ = Typ.t
type value = Value.t
type pattern = Pattern.t
type proc = Process.t
type term = Term.t
type element = string*bool option
type action = Action.t
type substitution = (string*value) list
type environment = Environment.t
type definition = Definition.t
type species = Species.t
type record = Record.t

let rec cases (v0:value) (l: (value*proc) list) =
  match l with
  | [] -> Process.Null
  | (v,p)::l ->
      match Value.case v0 v with
      | None -> cases v0 l
      | Some bindings -> Process.bind bindings p

let restrict (vs:value list) (t:term) =
  let (counter:int),(t:term) = Term.fresh t in
  let vs:value list = List.map Value.eval vs in
  let s:substitution = Value.res counter vs
  in s,t

let rec cons (p:proc) (t:term) =
  match p with
  | Process.Null -> t
  | Process.Parallel(ps) -> ((* Breadth-first search to avoid stack overflow with recursive match processes *)
    let rec f (ps:proc list) (t:term) = (
      match ps with
      | [] -> t
      | (Process.Match(v,l))::ps -> cons (Process.Match(v,l)) (f ps t)
      | p::ps -> f ps (cons p t)) in f ps t)
  | Process.New(vs,p) ->
      let (s:substitution),(t:term) = restrict vs t
      in cons (Process.bind s p) t
  | Process.Match(v,l) ->
      let v:value = Value.eval v in
      let p:proc = cases v l
      in cons p t
  | Process.Value(m,v,p) -> cons (Process.bind_pattern [v] [m] p) t
  | Process.Repeat(i,p) ->
      if i > 0
      then cons (Process.Repeat(i-1,p)) (cons p t)
      else t
  | Process.Instance(n,vs) ->
      match n,List.map Value.eval vs with
      | Value.Name("println"), [Value.String(s)] -> Io.println(s); t
      | Value.Name("print"), [Value.String(s)] -> Io.print(s); t
      | Value.Name("break"), [] -> Io.println("System paused. Press Enter to resume"); ignore(read_line()); t
      | _,_ ->
          let vs:value list = List.map Value.eval vs
          in match Term.find n t with
          | None ->  failwith ("undefined species " ^ Species.to_string(n,vs))
          | Some(m,d) ->
              let s:substitution = List.flatten (List.map2 Pattern.bind_value vs m)
              in match d with
              | Definition.Process(p) -> cons (Process.bind s p) t
              | Definition.Choice(c) -> Term.add (n,vs) (Choice.eval (Choice.bind s c)) t

let rec reduce (t:term) =
  match Term.gillespie t with
  | None -> None
  | Some((x:value),(index:float),(time:float)) ->
      match x with (*let t:term = Term.flip time x t in*)
      | Value.Float(_) -> (
        match Term.remove_delay x index t
        with
        | Some(i,v,Action.Delay(x),p,t) -> (
          let success,t' = Term.react "delay" infinity [i] [p] time t
          in
          Some(time, cons (Process.New(v,p)) t'))
        | _ -> failwith ("No delay on " ^ Value.to_string x))
      | Value.Channel(ch,_,_,rd,_) -> ( (* assumes channel x is not restricted *)
        match Term.remove_input x index t with
        | Some(i1,v1,Action.Input(x,m,rate1),p1,index,t) -> (
          match Term.remove_output x index t with
          | Some(i2,v2,Action.Output(x,n,rate2),p2,t) ->
              let success,t' = Term.react ch (Value.rate rd) [i1;i2] [p1;p2] time t in
              let (s:substitution),(t:term) = restrict v2 t' in
              let n:value list = List.map (Value.bind s) (List.map Value.eval n) in
              let p2:proc = Process.bind s p2 in
              let p1:proc = Process.bind_pattern n m p1 in
              let p:proc = Process.Parallel [Process.New(v1,p1);p2]
              (*let p1:proc = Process.bind_pattern (List.map Value.eval n) m p1 in *)
              (*let p:proc = Process.New(v1@v2, Process.Parallel [p1;p2])*)
          in Some(time,cons p t)
          | _ -> failwith ("Simulator.reduce: No output on " ^ Value.to_string x))
        | _ -> failwith ("Simulator.reduce: No input on " ^ Value.to_string x))
      | _ -> failwith ("No reaction on " ^ Value.to_string x)

let execute (duration:float) (resolution:int) (actions:action list) (speciess:species list) (file:string) (debug:bool) (t:term) =
  let delta:float =
    if (duration = infinity) || (resolution < 2) then 0.
    else duration /. (float_of_int resolution) in
  let line (time:float) (numbers:int list) =
    let f (i:int) (acc:string) = "," ^ string_of_int i ^ acc in
    string_of_float time ^ List.fold_right f numbers "\n" in
  let line_pos (time:float) rows =
    let display_pos (s:species) (id,p) acc =
      let x,y,z = p in
      (string_of_float time) ^ "," ^ (Species.to_string s) ^ "," ^
      id ^ "," ^
      (string_of_float x) ^ "," ^ (string_of_float y) ^ "," ^
      (string_of_float z) ^ "\n" ^ acc in
    let display_row acc (s,positions) =
      acc ^ List.fold_right (fun x acc -> display_pos s x acc) positions "" in
    List.fold_left display_row "" rows in
  Io.append_file (file ^ "_pos.csv") (Volume.spatial_info "#" (Term.get_volume t));
  let log (t:term) (time:float) =
    if (actions <> []) || (speciess <> []) then
      let numbers:int list = Term.plot actions speciess t in
      let cords = Term.plot_vol speciess t in
      let quantity:string =  line time numbers in
      let positions:string =  line_pos time cords in
      Io.append_file (file ^ ".csv") quantity &&
      Io.append_file (file ^ "_pos.csv") positions
    else false in
  let write_debug (t:term) =
    if debug then Io.write_file (file ^ ".txt") (Term.to_string t)
    else false in
  ignore(write_debug t);
  ignore(log t 0.0);
  let rec execute (time:float) (t:term) (delay:float) =
    match reduce t with
    | None -> ignore(write_debug t)
    | Some(delay',t) ->
        (*let b:bool = write_debug t in *)
        let delay:float = delay +. delay' in
        let time':float = time +. delay in
        if time' > duration then
          let _ = write_debug t in
          ()
        else if delay > delta then
          let _ = log t time' in
          execute time' t 0.
        else execute time t delay in
  execute 0. t 0.

  (*****************************************************************************)
