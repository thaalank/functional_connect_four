exception Quit

module Human : Player.Player = struct 

  (** [board_regions (x,y)] is a list of regions corresponding to
      0-indexed points in a board of [x] columns and [y] rows
  *)
  let board_regions (x, y) =
    let width, height = float_of_int ((3*x)+1), float_of_int ((3*y)+1) in
    let lst = ref [] in
    for pos_x = 0 to x - 1 do
      for pos_y = 0 to y - 1 do
        let r : Window.region = 
          {
            x = (2. +. (3. *. float_of_int pos_x)) /. width; 
            y = (2. +. (3. *. float_of_int pos_y)) /. height;
            width = 2. /. width;
            height = 2. /. height;
          } in
        lst := (r, (pos_x, pos_y)) :: !lst;
      done
    done;
    !lst

  let rec move st =
    let col = ref None in
    let cc : Input.click_collection = 
      (board_regions (7,6)) |> List.rev_map
        (fun (r, (x,_)) -> ({
             callback = (fun () -> col := Some x); region = r;
           } : Input.click_region)) in
    let rec loop () =
      let stat = Graphics.wait_next_event 
          [Graphics.Button_down; Graphics.Key_pressed] in
      if stat.button then begin Input.wait_until_up (); 
        Input.run_callbacks cc loop 
          (Window.relativize (stat.mouse_x, stat.mouse_y)) end
      else if stat.key = Char.chr 27 then raise Quit else loop () in
    let rec loop_2 () = loop ();
      match !col with | None -> raise Quit | Some c -> 
        begin 
          try State.drop begin if State.get_turn st = State.YellowTurn 
                then State.Yellow else State.Red end c st
          with | State.FullCol | State.InvalidCol | State.InvalidRow -> 
            loop_2 () 
        end in loop_2 ()
end

