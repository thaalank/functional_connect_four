type 'a t = {
  background : Graphics.color; 
  shapes : 'a;
  draw : 'a -> unit;
  input : Input.click_collection
}

(** [shape] are different drawable shapes *)
type shape =
  | Circle of Window.region * Draw.pen
  | CircleOutline of Window.region * Draw.pen
  | RectOutline of Window.region * Draw.pen
  | Rect of Window.region * Draw.pen
  | Text of Window.region * Draw.pen * string list
  | Comp of shape list

type screen = {
  background : Graphics.color;
  shapes : shape list
}

(** [draw_shape sh] draws shape [sh]
    in the case of [Comp lst], it draws each element in [lst] 
    from first to last
*)
let rec draw_shape = function
  | Circle (r, p) -> Draw.relative_circle r p
  | CircleOutline (r, p) -> Draw.relative_circle ~filled:false r p
  | RectOutline (r, p) -> Draw.relative_rect ~filled:false r p
  | Rect (r, p) -> Draw.relative_rect r p
  | Text (r, p, s) -> Draw.relative_text r p s
  | Comp (h::t) -> draw_shape h; draw_shape (Comp t)
  | Comp [] -> ()


let draw_screen scr = 
  Draw.relative_rect 
    {x = 0.5; y = 0.5; width = 1.5; height = 1.5} 
    {color = scr.background; width = 4};
  ignore (scr.shapes |> List.rev_map draw_shape)
