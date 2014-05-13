(*****************************************************************************)
let append_file (file_name:string) (file_contents:string) =
  try
    let oc:out_channel = open_out_gen [Open_wronly;Open_append;Open_nonblock] 0755 file_name in
    let _ = output_string oc file_contents in
    let _ = flush oc in
    let _ = close_out oc
    in true
  with e -> false

let print (s:string) =
  let _ = print_string s in
  let _ = append_file "log" s
  in flush stdout

let println (s:string) =
  let _ = print_newline (print_string s)in
  let _ = append_file "log" (s^"\n")
  in ()

let error (s:string) = println s

let write_file (file_name:string) (file_contents:string) =
try
  let oc:out_channel = open_out file_name in
  let _ = output_string oc file_contents in
  let _ = flush oc in
  let _ = close_out oc
  in true
with e -> println (Printexc.to_string e); false

(*****************************************************************************)
