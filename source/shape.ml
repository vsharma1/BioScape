type t =
    Sphere of float (* Sphere(radius) *)
  | Cuboid of float*float*float (* Cuboid(width*height*depth) *)
  | None

type glshape = int list (*GlList.t*)

let display (html:bool) (sh:t) =
  let _ = if html then "<br>" else "\n" in
  match sh with
  | Sphere(r) -> "sphere " ^ string_of_float r
  | Cuboid(w,h,d) ->
      "cuboid(w = " ^ string_of_float w ^
      ", h = " ^ string_of_float h ^
      ", d = " ^ string_of_float d ^ ")"
  | None -> "empty"

let to_string (sh:t) = display false sh
let compile (sh:t) =
  []
