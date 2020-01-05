
(** [click_region] is a region associated with a callback
    when clicked *)
type click_region = {region : Window.region; callback : unit -> unit}

(** [click_collection] is a collection of [click_regions] *)
type click_collection = click_region list

(** [run_callbacks cc pt] runs the first callback of 
    [cc] that the relative point [pt] lies in 
    and runs [f ()] if [pt] doesn't lie in any region in [cc] *)
val run_callbacks : click_collection -> (unit -> unit) -> float * float -> unit

(** [wait_until_up] waits until the user releases
    the mouse button *)
val wait_until_up : unit -> unit

(** [click_or_quit status func loop ] quits if the user hits escape,
    runs [func status] if the user has clicked and released their mouse 
    and runs [loop ()] otherwise *)
val click_or_quit : Graphics.status -> (unit -> unit) ->
  (Graphics.status -> unit) -> unit