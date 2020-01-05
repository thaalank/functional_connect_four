(** The representation type of a token. *)
type token = Yellow | Red | Empty 

(** The abstract representation type of a column. *)
type col 

(** The representation type of whose turn it is or if the game is over. *)
type turn = YellowTurn | RedTurn | RedWins | YellowWins | Tie

(** The abstract representation type of a state in the game. *)
type t 

(** Raised when when a column is full. *)
exception FullCol

(** Raised when an index is not a valid column index. *)
exception InvalidCol 

(** Raised when an index is not a valid row index. *)
exception InvalidRow

(** [make_start_state r c] is the State.t with a board of [r] rows, [c] columns,
    and with YellowTurn as the first turn .*)
val make_start_state : int -> int -> t

(** [get_token c_i r_i t] is the token in [t] at column [c_i] and row [r_i].
    Raises: InvalidCol if c_i is not a valid column index.
    Raises: InvalidRow if r_i is not a valid row index.  *)
val get_token : int -> int -> t -> token 

(** [get_turn t] is the turn at state [t]. *)
val get_turn : t -> turn

(** [get_board t] is the board at state [t] represented as a list of token 
    lists. *)
val get_board : t -> token list list

(** [drop token c_i t] is the state after dropping [token] at [col_i] 
    in state [t]. Fails with "not a valid token" if the winning conditions are 
    met but [token] is not a valid token to meet the winning conditions. *)
val drop : token -> int -> t -> t

(** [get_possible_moves t] is a list of all the possible col_i that a token
    can legally be dropped into in [t]. *)
val get_possible_moves : t -> int list 