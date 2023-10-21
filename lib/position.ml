(* map where the keys are the locations and the values are the pieces in that
   location*)

module Positions_map : Map.S with type key = int * int = Map.Make (struct
  type t = int * int

  (* TODO: finish implmenting compare function correctly *)
  let compare (x : t) (y : t) =
    let x1, x2 = x in
    let y1, y2 = y in
    if x1 > x2 then 1 else if x1 < x2 then -1 else 0
end)

module PositionOfPieces = struct
  let rec buildRowofMap (map : 'a Positions_map.t) (row : int) (column : int)
      (value : int) : 'a Positions_map.t =
    if column <= 8 then
      buildRowofMap
        (Positions_map.add (row, column) value map)
        row (column + 1) value
    else map

  let rec buildEmptyMap (map : 'a Positions_map.t) (row : int) (value : int) :
      'a Positions_map.t =
    if row <= 8 then
      buildEmptyMap (buildRowofMap map row 0 value) (row + 1) value
    else map

  let placePiece (row : int) (column : int) (color : int)
      (map : 'a Positions_map.t) : 'a Positions_map.t =
    let f opt = match opt with None -> Some color | Some a -> Some color in
    Positions_map.update (row, column) f map
end
