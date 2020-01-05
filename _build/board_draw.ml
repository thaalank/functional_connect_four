(** [token_to_shape tk r] is token [tk] as a shape in region [r] *)
let token_to_shape tk r =
  match tk with
  | State.Yellow -> Settings.get_shape YellowPiece r
  | State.Red -> Settings.get_shape RedPiece r
  | State.Empty -> Circle (r, {color = Graphics.rgb 180 180 180; width = 1})

(** 
   [board_shapes (x,y) board] is a composition of shapes representing
    board [board] of dimensions [(x,y)]
*)
let board_shapes (x, y) board =
  let width, height = float_of_int ((3*x)+1), float_of_int ((3*y)+1) in
  let lst = ref [] in
  for pos_x = 0 to x - 1 do
    for pos_y = 0 to y - 1 do
      let r : Window.region = 
        {
          x = (2. +. (3. *. float_of_int pos_x)) /. width; 
          y = (2. +. (3. *. float_of_int pos_y)) /. height;
          width = 2. /. width; height = 2. /. height;
        } in
      let border = Screen.Circle ({
          r with width = r.width +. 0.02; height = r.height +. 0.02;
        }, {color = Settings.get_alt_color Settings.Background; width = 1}) 
      in
      lst := Screen.Comp 
          [border; token_to_shape (State.get_token pos_x pos_y board) r] 
             :: !lst;
    done
  done;
  Screen.Comp !lst