(** 
   [Player] is a player of the Connect 4 game
*)
module type Player = sig
  (** [move game] is the game state after the current player has made a move *)
  val move : State.t -> State.t
end