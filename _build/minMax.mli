(** [get_min_max_c_i max_min token depth t] is the index of the column 
    that the min_max algorithm chooses to place the token into where [max_min]
    indicates whether the algorithm is maximizing or minimizing the score, 
    [token] indicates which token should be dropped at the column index, [depth]
    indicates the depth of the algorithm, and [t] indicates at which state the
    token should be dropped. Returns a random column index if the algorithm does
    not return a move. 
*)
val get_mini_max_c_i : bool -> State.token -> int -> State.t -> int  