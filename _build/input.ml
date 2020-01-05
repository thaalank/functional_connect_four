type click_region = {region : Window.region; callback : unit -> unit }

type click_collection = click_region list

let run_callbacks cc f pt =
  let rec loop = function
    | [] -> f ()
    | h::t when Window.within h.region pt -> h.callback ()
    | _ :: t -> loop t
  in loop cc

let wait_until_up () =
  ignore (Graphics.wait_next_event [Graphics.Button_up])

let click_or_quit (stat:Graphics.status) l f =
  if stat.button 
  then begin wait_until_up (); f stat end
  else if stat.key = Char.chr 27 then ()
  else l ()

