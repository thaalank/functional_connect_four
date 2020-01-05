type region = {
  width : float;
  height : float;
  x : float;
  y : float;
}

let within (r:region) (x, y) = 
  let reg_x = r.x in
  let reg_y = r.y in
  let half_width = (r.width /. 2.) in
  let half_height = r.height /. 2. in
  x >= reg_x -. half_width && x <= reg_x +. half_width 
  &&
  y >= reg_y -. half_height && y <= reg_y +. half_height


let window_dim () =
  float_of_int (Graphics.size_x ()), float_of_int (Graphics.size_y ())

let relativize (x,y) =
  let window_width, window_height = window_dim () in
  (float_of_int x) /. window_width, (float_of_int y) /. window_height
