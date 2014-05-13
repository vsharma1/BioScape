exception Illegal_character
type proc = Process.t
type action = Action.t
type token = Parser.token
val parse_file : string -> (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)
val parse_string : string -> (float*int*bool*bool* ((Action.t*string) list * (Species.t*string) list)) * (Environment.t * Volume.t * Process.t * string)
