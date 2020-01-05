open Window

let (~.) = int_of_float

type pen = {color : Graphics.color; width : int}

let ink (p:pen) =
  Graphics.set_color p.color;
  Graphics.set_line_width p.width

let relative_circle ?(filled=true) (r:region) (p:pen) =
  ink p;

  let window_width, window_height = window_dim () in
  let reg_x, reg_y = window_width *. r.x, window_height *. r.y in
  let diameter = min (window_width *. r.width) (window_height *. r.height) in
  begin if filled then Graphics.fill_circle else Graphics.draw_circle end 
    (~. reg_x) (~. reg_y) (~. (diameter /. 2.))

let relative_rect ?(filled=true) (r:region) (p:pen) =
  ink p;

  let window_width, window_height = window_dim () in
  let reg_x, reg_y = window_width *. r.x, window_height *. r.y in
  let reg_w, reg_h = window_width *. r.width, window_height *. r.height in
  begin if filled then Graphics.fill_rect else Graphics.draw_rect end
    (~. (reg_x -. (reg_w /. 2.))) (~. (reg_y -. (reg_h /. 2.))) 
    (~. reg_w) (~. reg_h)

let relative_text (r:region) (p:pen) lst =
  ink p;

  let window_width, window_height = window_dim () in
  let reg_x = ~. (r.x *. window_width) in
  let reg_y = ~. (r.y *. window_height) in

  let dims = lst |> List.rev_map (fun s -> s, Graphics.text_size s) in
  let total_height = List.fold_left (fun acc (_, (_, h)) -> acc + h) 0 dims in
  let bottom_height = reg_y - (total_height/2) in
  let curr = ref bottom_height in
  ignore
    begin
      dims |> List.map 
        (fun (s, (w, h)) -> 
           Graphics.moveto (reg_x - (w/2)) !curr; Graphics.draw_string s; 
           curr := !curr + h)
    end

let text_region (r:region) lst =
  let dims = lst |> List.rev_map (fun s -> Graphics.text_size s) in
  let total_height, widest = List.fold_left 
      (fun (acc_h, max_w) (w, h) -> acc_h + h, max max_w w ) (0, 0) dims in
  let window_width, window_height = window_dim () in
  {
    r with
    width = (float_of_int widest) /. window_width;
    height = (float_of_int total_height) /. window_height;
  }
