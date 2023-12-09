let rec generateMoveHelper (possibleMoves : (int * int) list) (v : int)
    (currentNum : int) : int * int =
  match possibleMoves with
  | (x, y) :: tail ->
      if currentNum <> v then generateMoveHelper tail v (currentNum + 1)
      else (x, y)
  | [] -> raise (Invalid_argument "no more moves :(")

let generateMove (possibleMoves : (int * int) list) : int * int =
  if List.length possibleMoves = 0 then
    raise (Invalid_argument "error, possible move list is 0")
  else
    let movePos = Random.int (List.length possibleMoves) in
    generateMoveHelper possibleMoves movePos 0

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

let evaluate_board board color =
  let score = ref 0 in
  let weight_matrix =
    [|
      [| 4; -3; 2; 2; 2; 2; -3; 4 |];
      [| -3; -4; -1; -1; -1; -1; -4; -3 |];
      [| 2; -1; 1; 0; 0; 1; -1; 2 |];
      [| 2; -1; 0; 1; 1; 0; -1; 2 |];
      [| 2; -1; 0; 1; 1; 0; -1; 2 |];
      [| 2; -1; 1; 0; 0; 1; -1; 2 |];
      [| -3; -4; -1; -1; -1; -1; -4; -3 |];
      [| 4; -3; 2; 2; 2; 2; -3; 4 |];
    |]
  in
  for i = 0 to 7 do
    for j = 0 to 7 do
      match Board.get_element i j board with
      | piece when piece = color -> score := !score + weight_matrix.(i).(j)
      | piece when piece <> Board.Empty ->
          score := !score - weight_matrix.(i).(j)
      | _ -> ()
    done
  done;
  !score

let generateMoveHard (valid_moves : (int * int) list) (board : Board.board)
    (color : Board.piece) : int * int =
  let simulate_move (x, y) =
    let simulated_board = Board.place_and_flip_pieces x y color board in
    let board_score = evaluate_board simulated_board color in
    let flipped_count = count_flipped_pieces board simulated_board color in
    (board_score, flipped_count, (x, y))
  in
  let moves_with_scores = List.map simulate_move valid_moves in
  let compare (score1, flips1, _) (score2, flips2, _) =
    let score_comp = compare score2 score1 in
    if score_comp = 0 then compare flips2 flips1 else score_comp
  in
  match List.sort compare moves_with_scores with
  | [] -> raise (Invalid_argument "no more moves :(")
  | (_, _, best_move) :: _ -> best_move
