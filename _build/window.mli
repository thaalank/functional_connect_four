(** [region] is a region of the screen, and can be offscreen.
    [width] is its width as a fraction of the screen's width
    [height] is its height as a fraction of the screen's height
    [x] is the position of the center of the region along the x axis
    as a fraction of the screen's width
    [y] is the position of the center of the region along the y axis
    as a fraction of the screen's width
    Representation Invariant: 
    [width] and [height] are both >= 0
*)
type region = {
  width : float;
  height : float;
  x : float;
  y : float;
}


(** [window_dim ()] are the dimensions of the window [(width, height)]*)
val window_dim : unit -> float * float


(** [within region (x,y)] is whether the relative location [(x,y)] 
    is in or on the border of [region] *)
val within : region -> float * float -> bool

(** [relativize (x,y)] is the corresponding relative point
    corresponding to [(x,y)], relative to the current window *)
val relativize : int * int -> float * float
