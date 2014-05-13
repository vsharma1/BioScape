type species = Species.t
type t = {
  d: int;
}

let reshape (w:int) (h:int) (sc:t) = ()
let rotx (delta:float) (sc:t) = ()
let roty (delta:float) (sc:t) = ()
let rotz (delta:float) (sc:t) = ()
let display (sc:t) = ()

let glutInit (w:int) (h:int) (sc:t) =
  let _ = Glut.init Sys.argv in
  ()
  (*
  Glut.initDisplayMode ~alpha:true ~double_buffer:true ~depth:true ();
  Glut.initWindowSize ~w:w ~h:h;
  let win = Glut.createWindow ~title:"SPiM" in
    let pos = 300.0, 300.0, 300.0, 1.0
    and red = 0.8, 0.1, 0.0, 1.0
    and green = 0.0, 0.8, 0.2, 1.0
    and blue = 0.2, 0.2, 1.0, 1.0 in

    GlLight.light ~num:0 (`position pos);
    List.iter Gl.enable
        [`cull_face;`lighting;`light0;`depth_test;`normalize];

    let _reshape ~w ~h =
        reshape w h sc
    in
    let _keyboard_callback ~key ~x ~y =
        let redisp = ref true in
        begin
        match (char_of_int key) with
        'q' -> exit 0
        | '-' -> () (*ignore(f.zoom <- f.zoom +. 0.1) *)
        | '+' -> () (*ignore(f.zoom <- f.zoom -. 0.1) *)
        | _  -> redisp := false
        end;
        if !redisp then Glut.postRedisplay ();
    in
    let _special_key_callback ~key ~x ~y =
        let delta = 5.0 in
        let redisp = ref true in
        begin
            match key with
            | Glut.KEY_LEFT  -> (roty (-. delta)  sc)
            | Glut.KEY_RIGHT -> (roty delta sc)
            | Glut.KEY_DOWN  -> (rotx (-. delta) sc)
            | Glut.KEY_UP    -> (rotx delta sc)
            | _ -> redisp := false
        end;
        if !redisp then Glut.postRedisplay ();
    in
    let rec _timedUpdate ~value =
        Glut.postRedisplay();
        Glut.timerFunc ~ms:20 ~cb:_timedUpdate ~value:0 in
    Glut.timerFunc ~ms:20 ~cb:_timedUpdate ~value:0;
    Glut.keyboardFunc ~cb:_keyboard_callback ;
    Glut.reshapeFunc ~cb:_reshape ;
    Glut.displayFunc ~cb:(fun () -> (display sc)) ;
    Glut.specialFunc ~cb:_special_key_callback
    *)

let init (width:int) (heigth:int) (species_to_plot:species list) =
  let init_state = { d = 1; } in
  glutInit width heigth init_state;
  init_state
