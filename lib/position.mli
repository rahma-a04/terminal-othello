module Positions_map : Map.S with type key = int * int

module type PositionOfPieces = sig
  val buildRowOfMap :
    'a Positions_map.t -> int -> int -> int -> 'a Positions_map.t

  val buildEmptyMap : 'a Positions_map.t -> int -> int -> int
  val placePiece : int -> int -> int -> 'a Positions_map.t
end
