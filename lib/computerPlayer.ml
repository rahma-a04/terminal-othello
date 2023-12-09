let rec generateMoveHelper (valid_moves : (int * int) list) (v : int)
    (currentNum : int) : int * int =
  match valid_moves with
  | (x, y) :: tail ->
      if currentNum <> v then generateMoveHelper tail v (currentNum + 1)
      else (x, y)
  | [] -> raise (Invalid_argument "no more moves :(")

let generateMove valid_moves board color =
  if List.length valid_moves = 0 then
    raise (Invalid_argument "error, possible move list is 0")
  else
    let movePos = Random.int (List.length valid_moves) in
    generateMoveHelper valid_moves movePos 0

let count_flipped_pieces original_board new_board color =
  let original_count = Board.count_pieces original_board color in
  let new_count = Board.count_pieces new_board color in
  new_count - original_count

let generateMoveMedium valid_moves board color =
  (*let valid_moves = Board.find_all_valid_moves color board in*)
  let simulate_move (x, y) =
    let simulated_board = Board.place_and_flip_pieces x y color board in
    let flipped_count = count_flipped_pieces board simulated_board color in
    (flipped_count, (x, y))
  in
  let moves_with_flips = List.map simulate_move valid_moves in
  let compare (flips1, _) (flips2, _) = compare flips2 flips1 in
  match List.sort compare moves_with_flips with
  | [] -> raise (Invalid_argument "no more moves :(")
  | (_, best_move) :: _ -> best_move

let generateMoveHard valid_moves board color =
  if List.length valid_moves = 0 then
    raise (Invalid_argument "error, possible move list is 0")
  else
    let movePos = Random.int (List.length valid_moves) in
    generateMoveHelper valid_moves movePos 0
