(** [color] are the different colors pieces and
    the background can be colored *)
type color =
  | Black
  | White
  | Red
  | Green
  | Blue
  | Yellow
  | Cyan
  | Magenta

(** [yellow_color] is a reference to the color of the
    yellow pieces.
    (This sounds dumb, but it makes sense when you consider
    that the State module defines pieces based on Yellow/Red) 
*)
let yellow_color = ref Yellow

(** [red_color] is a reference to the color of the
    red pieces.
    (This sounds dumb, but it makes sense when you consider
    that the State module defines pieces based on Yellow/Red) 
*)
let red_color = ref Red

(** [background_color] is a reference to the color of the
    background *)
let background_color = ref Blue

(** 
   [next_color color] is the next color in the sequence of colors
*)
let next_color = function
  | Black -> White
  | White -> Red
  | Red -> Green
  | Green -> Blue
  | Blue -> Yellow
  | Yellow -> Cyan
  | Cyan -> Magenta
  | Magenta -> Black

(** 
   [prev_color color] is the previous color in the sequence of colors
*)
let prev_color = function
  | Black -> Magenta
  | White -> Black
  | Red -> White
  | Green -> Red
  | Blue -> Green
  | Yellow -> Blue
  | Cyan -> Yellow
  | Magenta -> Cyan

(** [to_color color] is the [Graphics] color of [color] *)
let to_color = function
  | Black -> Graphics.black
  | White -> Graphics.white
  | Red -> Graphics.red
  | Green -> Graphics.green
  | Blue -> Graphics.blue
  | Yellow -> Graphics.yellow
  | Cyan -> Graphics.cyan
  | Magenta -> Graphics.magenta

(** [to_alt_color color] is the [Graphics] color of [color],
    except slightly differently colored. *)
let to_alt_color = function
  | Black -> Graphics.rgb 89 89 89
  | White -> Graphics.rgb 240 240 240
  | Red -> Graphics.rgb 150 0 0
  | Green -> Graphics.rgb 0 200 0
  | Blue -> Graphics.rgb 0 0 200
  | Yellow -> Graphics.rgb 200 200 0
  | Cyan -> Graphics.rgb 0 200 200
  | Magenta -> Graphics.rgb 200 0 200

(** [change_setting f s] applies [f] to setting [s] *)
let change_setting f s =
  s := f !s

(** [change_setting c s] sets setting [s] to value [c] *)
let set_setting c s =
  s := c

(** [style] is a piece style *)
type style =
  | Circle
  | CircleDeep
  | CircleFlat
  | TriangleUp
  | TriangleDown
  | FourX
  | FourPlus
  | FiveX
  | FivePlus
  | SixHexFlat
  | SixHexPointy
  | SevenHexFlat
  | SevenHexPointy
  | Smile

(** [next_style sty] is the next style in the sequence
    of styles *)
let next_style = function
  | Circle -> CircleDeep
  | CircleDeep -> CircleFlat
  | CircleFlat -> TriangleUp
  | TriangleUp -> TriangleDown
  | TriangleDown -> FourX
  | FourX -> FourPlus
  | FourPlus -> FiveX
  | FiveX -> FivePlus
  | FivePlus -> SixHexFlat
  | SixHexFlat -> SixHexPointy
  | SixHexPointy -> SevenHexFlat
  | SevenHexFlat -> SevenHexPointy
  | SevenHexPointy -> Smile
  | Smile -> Circle

(** [prev_style sty] is the previous style in the sequence
    of styles *)
let prev_style = function
  | Circle -> Smile
  | CircleDeep -> Circle
  | CircleFlat -> CircleDeep
  | TriangleUp -> CircleFlat
  | TriangleDown -> TriangleUp
  | FourX -> TriangleDown
  | FourPlus -> FourX
  | FiveX -> FourPlus
  | FivePlus -> FiveX
  | SixHexFlat -> FivePlus
  | SixHexPointy -> SixHexFlat
  | SevenHexFlat -> SixHexPointy
  | SevenHexPointy -> SevenHexFlat
  | Smile -> SevenHexPointy

(** [yellow_style] is a reference to the style of yellow pieces *)
let yellow_style = ref Circle

(** [red_style] is a reference to the style of red pieces *)
let red_style = ref Circle

(* [to_desc sty] is the description of [sty] *)
let to_desc = function
  | Circle -> "CIRCLE"
  | CircleDeep -> "DEEP CIRCLE"
  | CircleFlat -> "FLAT CIRCLE"
  | TriangleUp -> "UPWARDS TRIPLET"
  | TriangleDown -> "DOWNWARDS TRIPLET"
  | FourX -> "QUADRUPLET X"
  | FourPlus -> "QUADRUPLET PLUS"
  | FiveX -> "QUINTUPLET X"
  | FivePlus -> "QUINTUPLET PLUS"
  | SixHexFlat -> "FLAT-TOPPED HEXAGON"
  | SixHexPointy -> "POINTY-TOPPED HEXAGON"
  | SevenHexFlat -> "FLAT FLOWER"
  | SevenHexPointy -> "POINTY FLOWER"
  | Smile -> "SMILEY FACE :)"

(** [circ ~offset:(x_off, y_off) ~alt:alter factor r c] is a circle
    at [(r.x, r.y)] offset by [(x_off, y_off)] relative to 40% of the region's
    width and height. The size of the circle is scaled by [factor]. 
    The circle has an alternative color to [c] if [alt] is true. *)
let circ 
    (r:Window.region) c ?(offset=(0.0,0.0)) ?(alt=false) (factor:float) =
  let x_off, y_off = offset in
  Screen.Circle (
    {
      x = r.x +. (x_off*.r.width/.2.5);
      y = r.y +. (y_off*.r.height/.2.5);
      width = r.width *. factor; 
      height = r.height *. factor;
    }, 
    {
      color = (if alt then to_alt_color else to_color) c; 
      width = 1
    })

(** [border r c] is the border for each piece style *)
let border r c = Screen.Comp 
    [circ r c 1.0; circ r c ~alt:true 0.9; circ r c  0.8;]

(** [circle_deep r c] is a circle with the deep circle style 
    at [r] with color [c] *)
let circle_deep r c = 
  Screen.(Comp [border r c; circ r c ~alt:true 0.7; circ r c 0.6;])

(** [circle_flat r c] is a circle with the flat circle style 
    at [r] with color [c] *)
and circle_flat r c = Screen.(Comp [circ r c 1.0;])

(** [triangle_up r c] is a circle with the upwards triangle style 
    at [r] with color [c] *)
and triangle_up r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.0, 0.5) ~alt:true 0.28;
      circ r c ~offset:(0.0, 0.5) 0.23;

      circ r c ~offset:(-0.433, -0.25) ~alt:true 0.28;
      circ r c ~offset:(-0.433, -0.25) 0.23;

      circ r c ~offset:(0.433, -0.25) ~alt:true 0.28;
      circ r c ~offset:(0.433, -0.25) 0.23;
    ])

(** [triangle_down r c] is a circle with the downwards triangle style 
    at [r] with color [c] *)
and triangle_down r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.0, -0.5) ~alt:true 0.28;
      circ r c ~offset:(0.0, -0.5) 0.23;

      circ r c ~offset:(-0.433, 0.25) ~alt:true 0.28;
      circ r c  ~offset:(-0.433, 0.25) 0.23;

      circ r c  ~offset:(0.433, 0.25) ~alt:true 0.28;
      circ r c ~offset:(0.433, 0.25) 0.23;
    ])

(** [four_x r c] is a circle with the 4 circle X style 
    at [r] with color [c] *)
and four_x r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.3536, 0.3536) ~alt:true 0.22;
      circ r c ~offset:(0.3536, 0.3536) 0.17;

      circ r c ~offset:(0.3536, -0.3536) ~alt:true 0.22;
      circ r c ~offset:(0.3536, -0.3536) 0.17;

      circ r c ~offset:(-0.3536, 0.3536) ~alt:true 0.22;
      circ r c ~offset:(-0.3536, 0.3536) 0.17;

      circ r c ~offset:(-0.3536, -0.3536) ~alt:true 0.22;
      circ r c ~offset:(-0.3536, -0.3536) 0.17;
    ])

(** [four_plus r c] is a circle with the 4 circle plus style 
    at [r] with color [c] *)
and four_plus r c =
  Screen.(
    Comp [
      border r c;
      circ r c ~offset:(0.5, 0.0) ~alt:true 0.22;
      circ r c ~offset:(0.5, 0.0) 0.17;

      circ r c ~offset:(-0.5, 0.0) ~alt:true 0.22;
      circ r c ~offset:(-0.5, 0.0) 0.17;

      circ r c ~offset:(0.0, 0.5) ~alt:true 0.22;
      circ r c ~offset:(0.0, 0.5) 0.17;

      circ r c ~offset:(0.0, -0.5) ~alt:true 0.22;
      circ r c ~offset:(0.0, -0.5) 0.17;
    ]
  )

(** [five_x r c] is a circle with the 5 circle X style 
    at [r] with color [c] *)
and five_x r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.3536, 0.3536) ~alt:true 0.22;
      circ r c ~offset:(0.3536, 0.3536) 0.17;

      circ r c ~offset:(0.3536, -0.3536) ~alt:true 0.22;
      circ r c ~offset:(0.3536, -0.3536) 0.17;

      circ r c ~offset:(-0.3536, 0.3536) ~alt:true 0.22;
      circ r c ~offset:(-0.3536, 0.3536) 0.17;

      circ r c ~offset:(-0.3536, -0.3536) ~alt:true 0.22;
      circ r c ~offset:(-0.3536, -0.3536) 0.17;

      circ r c ~offset:(0.0, 0.0) ~alt:true 0.22;
      circ r c ~offset:(0.0, 0.0) 0.17;
    ])

(** [five_plus r c] is a circle with the 5 circle plus style 
    at [r] with color [c] *)
and five_plus r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.5, 0.0) ~alt:true 0.22;
      circ r c ~offset:(0.5, 0.0) 0.17;

      circ r c ~offset:(-0.5, 0.0) ~alt:true 0.22;
      circ r c ~offset:(-0.5, 0.0) 0.17;

      circ r c ~offset:(0.0, 0.5) ~alt:true 0.22;
      circ r c ~offset:(0.0, 0.5) 0.17;

      circ r c ~offset:(0.0, -0.5) ~alt:true 0.22;
      circ r c ~offset:(0.0, -0.5) 0.17;

      circ r c ~offset:(0.0, 0.0) ~alt:true 0.22;
      circ r c ~offset:(0.0, 0.0) 0.17;
    ])

(** [six_flat r c] is a circle with the 6 circle flat hexagon style 
    at [r] with color [c] *)
and six_flat r c =        
  Screen.(Comp [
      border r c ;
      circ r c ~offset:(-0.5, 0.0) ~alt:true 0.20;
      circ r c ~offset:(-0.5, 0.0) 0.15;

      circ r c ~offset:(0.25, -0.433) ~alt:true 0.20;
      circ r c ~offset:(0.25, -0.433) 0.15;

      circ r c ~offset:(0.25, 0.433) ~alt:true 0.20;
      circ r c ~offset:(0.25, 0.433) 0.15;

      circ r c ~offset:(0.5, 0.0) ~alt:true 0.20;
      circ r c ~offset:(0.5, 0.0) 0.15;

      circ r c ~offset:(-0.25, -0.433) ~alt:true 0.20;
      circ r c ~offset:(-0.25, -0.433) 0.15;

      circ r c ~offset:(-0.25, 0.433) ~alt:true 0.20;
      circ r c ~offset:(-0.25, 0.433) 0.15;
    ])

(** [six_pointy r c] is a circle with the 6 circle pointy hexagon style 
    at [r] with color [c] *)
and six_pointy r c =
  Screen.(
    Comp [
      border r c;
      circ r c ~offset:(0.0, -0.5) ~alt:true 0.20;
      circ r c ~offset:(0.0, -0.5) 0.15;

      circ r c ~offset:(-0.433, 0.25) ~alt:true 0.20;
      circ r c ~offset:(-0.433, 0.25) 0.15;

      circ r c ~offset:(0.433, 0.25) ~alt:true 0.20;
      circ r c ~offset:(0.433, 0.25) 0.15;

      circ r c ~offset:(0.0, 0.5) ~alt:true 0.20;
      circ r c ~offset:(0.0, 0.5) 0.15;

      circ r c ~offset:(-0.433, -0.25) ~alt:true 0.20;
      circ r c ~offset:(-0.433, -0.25) 0.15;

      circ r c ~offset:(0.433, -0.25) ~alt:true 0.20;
      circ r c ~offset:(0.433, -0.25) 0.15;
    ]
  )

(** [seven_flat r c] is a circle with the 7 circle flat hexagon style 
    at [r] with color [c] *)
and seven_flat r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(-0.5, 0.0) ~alt:true 0.2;
      circ r c ~offset:(-0.5, 0.0) 0.15;

      circ r c ~offset:(0.25, -0.433) ~alt:true 0.2;
      circ r c ~offset:(0.25, -0.433) 0.15;

      circ r c ~offset:(0.25, 0.433) ~alt:true 0.2;
      circ r c ~offset:(0.25, 0.433) 0.15;

      circ r c ~offset:(0.5, 0.0) ~alt:true 0.2;
      circ r c ~offset:(0.5, 0.0) 0.15;

      circ r c ~offset:(-0.25, -0.433) ~alt:true 0.2;
      circ r c ~offset:(-0.25, -0.433) 0.15;

      circ r c ~offset:(-0.25, 0.433) ~alt:true 0.2;
      circ r c ~offset:(-0.25, 0.433) 0.15;

      circ r c ~offset:(0.0, 0.0) ~alt:true 0.2;
      circ r c ~offset:(0.0, 0.0) 0.15;
    ])

(** [seven_pointy r c] is a circle with the 7 circle pointy hexagon style 
    at [r] with color [c] *)
and seven_pointy r c =
  Screen.(Comp [
      border r c;
      circ r c ~offset:(0.0, -0.5) ~alt:true 0.20;
      circ r c ~offset:(0.0, -0.5) 0.15;

      circ r c ~offset:(-0.433, 0.25) ~alt:true 0.20;
      circ r c ~offset:(-0.433, 0.25) 0.15;

      circ r c ~offset:(0.433, 0.25) ~alt:true 0.20;
      circ r c ~offset:(0.433, 0.25) 0.15;

      circ r c ~offset:(0.0, 0.5) ~alt:true 0.20;
      circ r c ~offset:(0.0, 0.5) 0.15;

      circ r c ~offset:(-0.433, -0.25) ~alt:true 0.20;
      circ r c ~offset:(-0.433, -0.25) 0.15;

      circ r c ~offset:(0.433, -0.25) ~alt:true 0.20;
      circ r c ~offset:(0.433, -0.25) 0.15;

      circ r c ~offset:(0.0, 0.0) ~alt:true 0.20;
      circ r c ~offset:(0.0, 0.0) 0.15;
    ])

(** [smile r c] is a circle with the smile style 
    at [r] with color [c] *)
and smile r c =
  Screen.(
    Comp [
      border r c;

      circ r c ~offset:(0.3536, 0.3536) ~alt:true 0.20;
      circ r c ~offset:(-0.3536, 0.3536) ~alt:true 0.20;

      circ r c ~offset:(-0.433, -0.25) ~alt:true 0.20;
      circ r c ~offset:(0.433, -0.25) ~alt:true 0.20;

      circ r c ~offset:(0.25, -0.433) ~alt:true 0.20;
      circ r c ~offset:(-0.25, -0.433) ~alt:true 0.20;

      circ r c ~offset:(0.0, -0.5) ~alt:true 0.20;
    ]
  )

(** [to_shape c sty r] is a shape with color [c], style [sty],
    occupying region [r] *)
let to_shape c sty (r:Window.region) = 
  begin
    match sty with
    | Circle -> border
    | CircleDeep -> circle_deep
    | CircleFlat -> circle_flat 
    | TriangleUp -> triangle_up 
    | TriangleDown -> triangle_down 
    | FourX -> four_x 
    | FourPlus -> four_plus 
    | FiveX -> five_x
    | FivePlus -> five_plus
    | SixHexFlat -> six_flat
    | SixHexPointy -> six_pointy
    | SevenHexFlat -> seven_flat
    | SevenHexPointy -> seven_pointy
    | Smile -> smile
  end r c

type selection =
  | YellowSelection
  | RedSelection
  | Background

(** [to_ref sel] is the reference [sel] corresponds to *)
let to_ref = function
  | YellowSelection -> yellow_color
  | RedSelection -> red_color
  | Background -> background_color

let advance_color sel =
  change_setting next_color (to_ref sel)

let devance_color sel =
  change_setting prev_color (to_ref sel)

let reset_color sel =
  set_setting 
    begin
      match sel with
      | YellowSelection -> Yellow
      | RedSelection -> Red
      | Background -> Blue
    end
    (to_ref sel)

let get_color sel =
  to_color (!(to_ref sel))

let get_alt_color sel =
  to_alt_color (!(to_ref sel))

type piece =
  | YellowPiece
  | RedPiece

(** [to_ref' pi] is the reference corresponding to piece [pi] *)
let to_ref' = function
  | YellowPiece -> yellow_style
  | RedPiece -> red_style

let advance_style pi =
  change_setting next_style (to_ref' pi)

let devance_style pi =
  change_setting prev_style (to_ref' pi)

let reset_style pi =
  set_setting 
    begin
      match pi with
      | YellowPiece -> Circle
      | RedPiece -> Circle
    end
    (to_ref' pi)

let get_shape pi =
  let c, sty = 
    begin
      match pi with
      | RedPiece -> !red_color, !red_style
      | YellowPiece -> !yellow_color, !yellow_style
    end
  in
  to_shape c sty

let get_desc pi =
  to_desc (!(to_ref' pi))