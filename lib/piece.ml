(*module Positions_map*)
module Positions_map : Map.S with type key = int * int = Map.Make (struct
  (*Copied this here from board.ml temporarily*)
  type t = int * int

  let compare (x : t) (y : t) =
    let x1, x2 = x in
    let y1, y2 = y in
    if x1 > x2 then 1 else if x1 < x2 then -1 else 0
end)

type piece =
  | Black
  | White

(*module Position = struct type t = int * int end*)
(* 8 functions to output piece option lists of the *)
module LegitMove = struct
  let rec northeast curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: northeast (curr_x + 1) (curr_y + 1)

  let rec east curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: east (curr_x + 1) curr_y

  let rec southeast curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: southeast (curr_x + 1) (curr_y - 1)

  let rec south curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: south curr_x (curr_y - 1)

  let rec southwest curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: southwest (curr_x - 1) (curr_y - 1)

  let rec west curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: west (curr_x - 1) curr_y

  let rec northwwest curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: northwwest (curr_x - 1) (curr_y + 1)

  let rec north curr_x curr_y =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else (curr_x, curr_y) :: north curr_x (curr_y + 1)

  let rec can_black_play_helper = function
    | Some Black :: _ -> true
    | Some White :: tail -> can_black_play_helper tail
    | _ -> false

  let can_black_play = function
    | Some White :: tail -> can_black_play_helper tail
    | _ -> false

  let rec can_white_play_helper = function
    | Some White :: _ -> true
    | Some Black :: tail -> can_white_play_helper tail
    | _ -> false

  let can_white_play = function
    | Some Black :: tail -> can_white_play_helper tail
    | _ -> false

  let is_legit board curr_x curr_y piece =
    let f n =
      (match piece with
      | Black -> can_black_play
      | White -> can_white_play)
        (List.map (fun (x, y) -> Positions_map.find_opt (x, y) board) n)
    in
    f (northeast (curr_x + 1) (curr_y + 1))
    || f (east (curr_x + 1) curr_y)
    || f (southeast (curr_x + 1) (curr_y - 1))
    || f (south curr_x (curr_y - 1))
    || f (southwest (curr_x - 1) (curr_y - 1))
    || f (west (curr_x - 1) curr_y)
    || f (northwwest (curr_x - 1) (curr_y + 1))
    || f (north curr_x (curr_y + 1))
end
