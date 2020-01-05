(** Raised when a player wants to quit *)
exception Quit

(** A human player. 
    [move] raises [Quit] when the player wants to quit.
*)
module Human : Player.Player 