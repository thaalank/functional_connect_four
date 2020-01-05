
(** [t] is a screen with background color [t.background],
    a collection of shapes [t.shapes] and a way to draw them
    [t.draw], and a collection of inputs [t.input] *)
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

(** 
   [screen] is the visual appearance of a screen
*)
type screen = {
  background : Graphics.color;
  shapes : shape list
}

(** [draw_screen scr] colors the background [scr.background] and
    draws each shape in [scr.shapes] in order *)
val draw_screen : screen -> unit

