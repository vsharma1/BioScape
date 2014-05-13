val common : 'a list -> 'a list -> ('a list * 'a list * 'a list)
type pos = float*float*float
val distance_vector : pos -> pos -> float
val avg_vector : pos -> pos -> pos
