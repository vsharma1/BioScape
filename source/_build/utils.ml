type pos = float*float*float

let rec common l1 l2 =
  match l1 with
    [] -> ([], [], l2)
  | h1::t1 -> (
    match l2 with
      [] -> (l1, [], [])
    | h2::t2 when h1 < h2 ->
        let left,c,right = common t1 l2 in
        (h1::left,c,right)
    | h2::t2 when h1 > h2 ->
        let left,c,right = common l1 t2 in
        (left,c,h2::right)
    | h2::t2 -> (
      match common t1 t2 with
        left,[],right -> (left, [h1], right)
      | left,h3::t3,right when h3 = h1 -> (left,h3::t3,right)
      | left,h3::t3,right -> (left,h1::h3::t3,right)
    )
  )

let distance_vector (a:pos) (b:pos) =
  let xa,ya,za = a in
  let xb,yb,zb = b in
  let sq x = x *. x in
  sqrt (
    (sq (xa-.xb)) +.
    (sq (ya-.yb)) +.
    (sq (za-.zb)))

let avg_vector (a:pos) (b:pos) =
  let xa,ya,za = a in
  let xb,yb,zb = b in
  (
    (xa +. xb) /. 2.,
    (ya +. yb) /. 2.,
    (za +. zb) /. 2.)
