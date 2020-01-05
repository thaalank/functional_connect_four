open Window

(** [pen] is a drawing pen with color [pen.color] and width [pen.width] 
    Representation Invariant: [pen.width] >= 0 
*)
type pen = {color : Graphics.color; width : int}

(** [ink pen] sets the drawing color to [pen.color] and the
    line width to [pen.width] *)
val ink : pen -> unit

(** [relative_circle region pen ?filled] draws using [pen] as large of a circle
    as possible within [region] at [(region.x, region.y)], and fills it
    if [filled] is true *)
val relative_circle : ?filled:bool -> region -> pen -> unit

(** [relative_rect region pen] is the rectangle formed from [region]
    drawn with [pen], and fills it if [filled] is true *)
val relative_rect : ?filled:bool -> region -> pen -> unit

(** [relative_text region pen str] draws [str] at [(region.x, region.y)] 
    using [pen] *)
val relative_text : region -> pen -> string list -> unit

(** [text_region region lst] is the region occupied by
    [lst] such that the height of the region is the total
    height of each string in [lst] and the width is
    the maximum width among the strings in [lst] *)
val text_region : region -> string list -> region
