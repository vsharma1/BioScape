type position = Lexing.position
type typ = Typ.t
type name = string
type env = Env.t
type op =
    Plus
  | Minus
  | Mul
  | Div
  | Equal
  | Different
  | Lt
  | Gt
  | Ltequal
  | Gtequal

type fn =
    Float2Int
  | Int2Float
  | Sqrt

type t =
    String of string
  | Int of int
  | Channel of (string*int*t*t*typ)
  | Process of (string*int*typ)
  | Bool of bool
  | Char of char
  | Float of float
  | Data of name*t list
  | Cons of t*t
  | Name of name
  | Op of (t*op*t)
  | Neg of t
  | Show of t
  | Function of (fn*t)
  | Tuple of t list
  | List of t list
  | Coordinate of float*float*float
  | Space of Shape.t*t

type substitution = (name*t) list

val origin : t
val empty : t
val init : name -> t
val compare : t -> t -> int
val display : bool -> t -> string
val displays : bool -> t list -> string
val to_string : t -> string
val to_strings : t list -> string
val to_html : t -> string
val free_names : t -> name list

val bind : substitution -> t -> t
val bindt : name -> typ -> t -> t
val source : t -> t
val channels : t list -> name list
(*val restrict : name -> int -> t -> typ -> t option*)
val res : int -> t list -> substitution

val eval : t -> t
val typecheck : env -> t -> typ
val case : t -> t -> ((name*t) list) option
val case_type : env -> typ -> t -> (name * typ) list
val rate : t -> float
val merge : t -> t -> t
(*****************************************************************************)
