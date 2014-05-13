type t
val empty : t
val to_string : string -> t -> string
val delays : t -> float
val inputs : t -> float
val outputs : t -> float
val mixed : t -> float
val plus : t -> t -> t
val minus : t -> t -> t
val add_input : float -> t -> t
val add_output : float -> t -> t
val add_delay : float -> t -> t
