type t =
    Sphere of float (* Sphere(radius) *)
  | Cuboid of float*float*float (* Cuboid(width*height*depth) *)
  | None

type glshape

val display : bool -> t -> string
val to_string : t -> string

val compile : t -> glshape
