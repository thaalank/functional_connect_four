(** [selection] is either the yellow pieces,
    the red pieces, or the background for color changing. *)
type selection =
  | YellowSelection
  | RedSelection
  | Background

(** [advance_color sel] advances the color of the selection
    to the next color. *)
val advance_color : selection -> unit

(** [devance_color sel] retreats the color of the selection
    to the previous color. *)
val devance_color : selection -> unit

(** [reset_color sel] resets the color of the selection.
    red for red pieces;
    yellow for yellow pieces;
    blue for the background *)
val reset_color : selection -> unit

(** [get_color sel] gets the [Graphics] color of the selection *)
val get_color : selection -> Graphics.color

(** [get_alt_color sel] gets the alternative 
    [Graphics] color of the selection *)
val get_alt_color : selection -> Graphics.color

(** [piece] is a choice of the red or yellow pieces *)
type piece =
  | YellowPiece
  | RedPiece

(** [advance_style pi] advances the style of [pi] to the
    next style *)
val advance_style : piece -> unit

(** [devance_color sel] retreats the style of [pi]
    to the previous style *)
val devance_style : piece -> unit

(** [reset_style pi] resets the style of [pi] to CIRCLE *)
val reset_style : piece -> unit

(** [get_shape pi r] is the [Screen] shape corresponding to
    [pi], with the style and color of [pi], within [r] *)
val get_shape : piece -> Window.region -> Screen.shape

(** [get_desc pi] is a description of the style of [pi] *)
val get_desc : piece -> string