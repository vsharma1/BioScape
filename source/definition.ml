(*****************************************************************************)
type value = Value.t
type proc = Process.t
type choice = Choice.t
type t =
  | Process of proc
  | Choice of choice
type substitution = (string*value) list

let display (html:bool) (d:t) =  match d with
  | Process(p) -> Process.display html p
  | Choice(c) -> Choice.display html c

let to_string (k:t) = display false k
let to_html (k:t) = display true k

let bind (s:substitution) (d:t) = match d with
  | Process(p) -> Process(Process.bind s p)
  | Choice(c) -> Choice(Choice.bind s c)
(*****************************************************************************)

