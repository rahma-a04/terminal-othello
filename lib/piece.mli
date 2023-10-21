open Map

module type Positions_mapType = sig
  type t = int * int

  val find_opt : t -> t -> int
end

module type LegitMoveType = sig
  type piece = Black | White

  val northeast : int -> int -> (int * int) list
  val east : int -> int -> (int * int) list
  val southeast : int -> int -> (int * int) list
  val south : int -> int -> (int * int) list
  val southwest : int -> int -> (int * int) list
  val west : int -> int -> (int * int) list
  val northwwest : int -> int -> (int * int) list
  val north : int -> int -> (int * int) list
  val can_black_play_helper : piece option list -> bool
  val can_black_play : piece option list -> bool
  val can_white_play_helper : piece option list -> bool
  val can_white_play : piece option list -> bool
  val is_legit : piece Positions_map.t -> int -> int -> piece -> bool
  (*  val is_legit : Positions_map_sig.key Map.t -> int -> int -> piece -> bool
*)
end

(*module Positions_map : Positions_mapType
  module LegitMove : LegitMoveType*)
