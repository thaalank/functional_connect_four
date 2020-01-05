(** [Easy] is an AI player that randomly places tokens. *)
module Easy : Player.Player = struct
  open State
  open Random

  let rec move t = 
    let rand_col_i = Random.int(7) in 
    try 
      begin
        match get_turn t with 
        | YellowTurn -> drop Yellow rand_col_i t 
        | RedTurn -> drop Red rand_col_i t 
        | _ -> t 
      end
    with FullCol -> move t 
end

(** [Medium] is an AI player that places tokens based on a MinMax Alg. with 
    depth 3. *)
module Medium : Player.Player = struct
  open State
  open MinMax

  let rec move t = 
    try 
      begin
        match get_turn t with 
        | YellowTurn -> 
          let col_i = get_mini_max_c_i true Yellow 3 t in 
          drop Yellow col_i t 
        | RedTurn -> 
          let col_i = get_mini_max_c_i true Red 3 t in 
          drop Red col_i t 
        | _ -> t 
      end
    with FullCol -> move t 
end

(** [Hard] is an AI player that places tokens based on a MinMax Alg. with 
    depth 5. *)
module Hard : Player.Player = struct
  open State
  open MinMax

  let rec move t = 
    try 
      begin
        match get_turn t with 
        | YellowTurn -> 
          let col_i = get_mini_max_c_i true Yellow 5 t in 
          drop Yellow col_i t 
        | RedTurn -> 
          let col_i = get_mini_max_c_i true Red 5 t in 
          drop Red col_i t 
        | _ -> t 
      end
    with FullCol -> move t 
end