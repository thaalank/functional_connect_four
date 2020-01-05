let checker () = 
  at_exit Graphics.close_graph;
  let rec speak = function
    | [] -> ()
    | h :: t -> print_endline h; speak t
  in
  speak (Utils.read_lines "check.txt");
  Graphics.open_graph "";
  let rect : Window.region = {width = 0.3; height = 0.2; x = 0.4; y = 0.6} in
  Draw.relative_rect ~filled:true rect {color = Graphics.blue; width = 2;};
  ignore (Graphics.wait_next_event 
            [Graphics.Button_down; Graphics.Key_pressed])


let () = checker ()