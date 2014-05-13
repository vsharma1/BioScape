
type pos = Utils.pos
type species = Species.t
type value = Value.t
type shape = Shape.t
module Map = Map.Make(Species)

type spatial_info = {
  shape: shape;
  bounds: value;
  step: float;
  counter: int;
  locations: (string*pos) list;
}

type t = spatial_info Map.t

let find (i:species) h =
  try Some(Map.find i h)
  with Not_found -> None


let empty = Map.empty
let update_step (s:species) (r:float) (vol:t) =
  match find s vol with
  | None -> Map.add s {
    shape = Shape.None;
    step = r;
    bounds = Value.Space (Shape.None,Value.origin);
    counter = 0;
    locations = []} vol
  | Some(e) -> Map.add s {e with step = r} vol

let add_movement (s:species) (r:float) (vol:t) = update_step s r vol

let format_id (v,vs) counter = (Value.to_string v) ^ (string_of_int counter)

let add_spatialinfo (s:species) (bounds:value) (sh:shape) (st:float) (v:t) =
  Io.println(
    "Space: " ^ Species.to_string s ^ "::" ^ Shape.to_string sh ^
    "::" ^ Value.to_string bounds ^ "::" ^ (string_of_float st));
  Map.add s {shape = sh; step = st; bounds = bounds; counter = 0; locations=[]} v

let add (s:species) (v:t) =
  let add (s':species) (si:spatial_info) =
    let random_point_in_space (space:Value.t) =
      match space with
      | Value.Space(sh, pos) -> (
        let offset (ox:float) (oy:float) (oz:float) = (match pos with
        | Value.Coordinate(x, y, z) -> (x +. ox, y +. oy, z +. oz)
        | _ -> failwith("Coordinate invalid: " ^ (Value.to_string pos))) in
        match sh with
        | Shape.Sphere(r) ->
            let r = Random.float r in
            let theta = Random.float 360.0 in
            let phi = Random.float 360.0 in
            offset (r *. (cos phi) *. (sin theta))
            (r *. (sin phi) *. (sin theta))
            (r *. (cos theta))
        | Shape.Cuboid(w,h,d) -> offset (Random.float w)
        (Random.float h)
        (Random.float d)
        | Shape.None -> failwith("Shapeless"))
      | _ -> failwith("Space not valid: " ^ (Value.to_string space)) in
    if Species.matches s s'
    then
      let p = random_point_in_space si.bounds in
      {
        si with
        locations = si.locations @ [format_id s si.counter, p];
        counter = si.counter + 1}
    else si in
  Map.mapi add v

let remove_one (s:species) (v:t) =
  let remove_one (s':species) (si:spatial_info) ((v':t), (p:(string*pos) option)) =
    if (Species.matches s s')
    then (Map.add s {
      si with
      locations = List.tl si.locations} v',
      Some(List.hd si.locations))
    else (Map.add s' si v', p) in
  let (v':t), (p:(string*pos) option) = Map.fold remove_one v (Map.empty, None) in
  match p with
  | None -> failwith("No Species " ^ (Species.to_string s) ^ " to remove")
  | Some(pos) -> (v', pos)

let display (html:bool) (v:t) =
  let indent = " " in
  let symbol (s:string) = if html then "<font color=#750000>" ^ s ^"</font>" else s in
  let newline = if html then "<br>" else "\n" in
  let display_coords x y z =
    symbol "(" ^
    string_of_float x ^ "," ^
    string_of_float y ^ "," ^
    string_of_float z ^
    symbol ")" in
  let f (s:species) (si:spatial_info) acc =
    let flocs acc (p:(string*pos)) =
      let id, (x,y,z) = p
      in
      acc ^ indent ^ id ^ "::" ^ display_coords x y z ^
      newline in
    List.fold_left flocs acc si.locations in
  Map.fold f v ""

let debug_reaction (reaction:string) (v:t) =
  ()
  (*Io.println("Reaction: " ^ reaction);
  Io.println("volume:");
  Io.println(display false v)*)

let random_walk p step bounds =
  let r = Random.float (step) in
  let theta = Random.float 360.0 in
  let phi = Random.float 360.0 in
  let id, pos = p in
  let x, y, z = pos in
  let put_in_bounds x y z =
    let float_modulo a b =
      a >= b, float_of_int (
        (int_of_float (a *. 1000.))
        mod
        (int_of_float (b *.  1000.))
      ) /. 1000. in
    match bounds with
    | Value.Space(Shape.None, Value.Float(0.)) -> false, (x, y, z)
    | Value.Space(Shape.Cuboid(w,h,d), Value.Coordinate(ox,oy,oz)) ->
        let wx, x' =  float_modulo (x +. (r *. (cos phi) *. (sin theta))) w in
        let wy, y' =  float_modulo (y +. (r *. (sin phi) *. (sin theta))) h in
        let wz, z' =  float_modulo (z +. (r *. (cos theta))) d in
        wx || wy || wz, (x' +. ox, y' +. oy, z' +. oz)
    | Value.Space(Shape.Sphere(r), Value.Coordinate(ox,oy,oz)) ->
        failwith("Spherical bounds not supported YET")
    | _ -> failwith("Not sure how we got here") in
  let wrapped, pos = put_in_bounds x y z in
  id, wrapped, pos

let move (delay:float) (v:t) =
  let reactions = ref [] in
  let move (s:species) (si:spatial_info) =
    if si.step <= 0. || List.length si.locations == 0
    then
      si
  else
    (
      let this_reaction = (string_of_int (List.length si.locations)
      ^ " * mov." ^ (Species.to_string s) ^ " @ "
      ^ (string_of_float si.step)) in
      reactions := this_reaction :: !reactions;
      let locs', ctr = List.fold_left (fun (acc,ctr) (p:(string*pos)) ->
        let id, wrapped, pos' = random_walk p si.step si.bounds in
        if wrapped
        then
          ((((format_id s ctr), pos') :: acc), (ctr + 1))
        else
          ((id, pos') :: acc), ctr) ([],si.counter) si.locations in
      {si with locations = locs'; counter = ctr}) in
  let v' = Map.mapi move v in
  if List.length !reactions > 0
  then
    debug_reaction (String.concat ", " !reactions) v';
  v'

let remove_closest (radius:float) (r1:species) (r2:species) (v:t) =
  let r1_list =
    match find r1 v with
    | None -> failwith("remove_closest: Species " ^ (Species.to_string r1) ^ " not found")
    | Some(si) -> si.locations in
  let r2_list =
    match find r2 v with
    | None -> failwith("remove_closest: Species " ^ (Species.to_string r2) ^ " not found")
    | Some(si) -> si.locations in
  let assumed_min = radius in
  let min, p1, p2 = (
    List.fold_left
    (fun (last_min, min_p1, min_p2) (id1, x1) -> (
      List.fold_left
      (fun (last_min', min_p1', min_p2') (id2, x2) ->
        let dist = Utils.distance_vector x1 x2 in
        if dist < last_min'
        then (dist, Some(x1), Some(x2))
        else (last_min', min_p1', min_p2'))
      (last_min, min_p1, min_p2)
      r2_list)
    )
    (assumed_min,None,None)
    r1_list) in
  (*Io.println("DEBUG: reaction radius: " ^ string_of_float min);*)
  let add_all_but (s:species) (p:pos) (v:t) =
    let add_all_but (key:species) (si:spatial_info) =
      if (Species.matches s key)
      then
        let locs, found =
          List.fold_left (fun (acc,found) idpos' ->
            let id, p' = idpos' in
            if not found && p' == p
            then (acc, true)
            else (idpos'::acc, found))
          ([], false)
          si.locations in
        assert found;
        { si with locations=locs }
      else
        si in
    Map.mapi add_all_but v in
  match p1,p2 with
  | None, None ->
      Io.println(
        "WARNING: No pair of " ^ (Species.to_string r1) ^ " & " ^
        (Species.to_string r2) ^
        " within the reaction radius: " ^ string_of_float radius);
      (None, v)
  | Some(p1), Some(p2) ->
      (Some(p1, p2), add_all_but r2 p2 (add_all_but r1 p1 v))
  | _ -> failwith("incomplete pair, code bug")

let react (ch:string) (radius:float) (reactants:species list) (products:species list) (delay:float) (v:t) =
  let species_to_str l =
    List.fold_left
    (fun acc x -> acc ^ Species.to_string x)
    ""
    l in
  let reaction = (species_to_str reactants) ^
  " -> " ^ (species_to_str products) ^ "@" ^ ch ^ "(" ^ string_of_float radius ^
  ")" in
  let reactants = List.sort compare reactants in
  assert (List.length reactants <= 2);
  let products = List.sort compare products in
  let add_at (s:species) (p:pos) (v:t) =
    let randomize p bounds =
      let id, wrapped, pos = random_walk p (min radius 1.0) bounds in
      (id, pos) in
    let updated =
      match find s v with
      | None ->
          let fake_bound = Value.Space(Shape.None, Value.Float(0.)) in
          (* internal process 1().. *)
          {
            shape=Shape.None;
            step=0.;
            bounds=fake_bound;
            counter=1;
            locations=[randomize (format_id s 0, p) fake_bound]}
      | Some(si) ->
          {
            si with
            locations = (randomize ((format_id s si.counter), p) si.bounds) :: si.locations;
            counter = si.counter + 1} in
    Map.add s updated v in
  let first_order_reaction (r:species) =
    let (v':t),(id, p) = remove_one r v in
    List.fold_left (fun acc x -> add_at x p acc) v' products in
  let second_order_reaction () =
    let r1,r2 = (List.nth reactants 0), (List.nth reactants 1) in
    let pair,v' = remove_closest radius r1 r2 v in
    match pair with
    | None -> false, v'
    | Some(x1, x2) ->
        let avg_pos = Utils.avg_vector x1 x2 in
        true, List.fold_left (fun acc x -> add_at x avg_pos acc) v' products in
  debug_reaction reaction v;
  let success, v' = (
    if compare reactants products == 0
    then true, v
    else if (List.length reactants) == 1
    then true, first_order_reaction (List.hd reactants)
    else second_order_reaction ()) in
  debug_reaction reaction v';
  success, move delay v'

let plot (speciess:species list) (v:t) =
  let fix (s:species) =
    let v,vs = s in
    match v with
    | Value.Name(x) -> (Value.Process(x,0,Typ.Void), vs)
    | _ -> assert false in
  let speciess = List.map fix speciess in
  let get_rows_for (s:species) =
    match find s v with
    | None -> s, []
    | Some(si) -> s, si.locations in
  List.map get_rows_for speciess

let spatial_info (prefix:string) (v:t) =
  let f (s:species) (si:spatial_info) acc =
    prefix ^ Species.to_string s ^ "::" ^ Shape.to_string si.shape ^
    "::" ^ Value.to_string si.bounds ^ "::" ^ (string_of_float si.step) ^ "\n"
    ^ acc in
  Map.fold f v ""
