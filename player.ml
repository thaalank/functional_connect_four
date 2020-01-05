module type Player = sig
  val move : State.t -> State.t
end