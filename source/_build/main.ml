(*
 * Check polymorphic channels
 * Rename process definitions to avoid name clashes
*)

(*****************************************************************************)
type proc = Process.t
type term = Term.t
type action = Simulator.action
type substitution = string
type species = Simulator.species
type position = Lexing.position
type environment = Environment.t
type volume = Volume.t

let format (pos:position) =
  "Line " ^ string_of_int pos.Lexing.pos_lnum ^
  " char " ^ string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ^ ": "

let run (file:string) =
  let _ = Random.self_init() in
  let ((duration:float),(resolution:int),(graph:bool),(debug:bool),plots),((e:environment),(vol:volume),(p:proc),(network:string)) = Lexer.parse_file file in
  let (actions:action list),(headers:string list) = List.split (fst plots) in
  let (speciess:species list),(headers':string list) = List.split (snd plots) in
  let headers = headers@headers' in
  let _ = file ^ ".csv" in
    Io.println ("SPiM v0.05 simulating " ^ file);
  let t':term = Simulator.cons p (Term.init e vol) in
  let t:term = Term.init_volume t' in
  let _ = if graph then Io.write_file (file ^ ".dot") network (*(Term.to_graph t)*) else false in
  let _ =
    if headers <> [] then
      let f (s:string) (acc:string) = "," ^ "\"" ^ s ^ "\"" ^ acc in
      let line:string = "#Time" ^ List.fold_right f headers "\n" in
      Io.write_file (file ^ ".csv") line && Io.write_file (file ^ "_pos.csv") line
    else Io.write_file (file ^ ".csv") ""
  in
  (*let _ = Scene.init 750 500 speciess in *)
  let _ = Simulator.execute duration resolution actions speciess file debug t in
  Io.println "Simulation finished."

let finish () =
  let _ = Io.println "Press Enter to quit... " in
  let _ = read_line()
  in Io.print "Bye"

let _ =
  try run Sys.argv.(1)
  with
      Invalid_argument s -> Io.println "Specify a file to simulate" ; finish ()
    | Typ.Error(pos,s) -> Io.println (format pos ^ s); finish ()
    | Failure s -> Io.println s; finish ()
    | e -> Io.println (Printexc.to_string e); finish ()

(*****************************************************************************)
