let rec generateMoveHelper (valid_moves : (int * int) list) (v : int)
    (currentNum : int) : int * int =
  match valid_moves with
  | (x, y) :: tail ->
      if currentNum <> v then generateMoveHelper tail v (currentNum + 1)
      else (x, y)
  | [] -> raise (Invalid_argument "no more moves :(")

let generateMoveEasy (valid_moves : (int * int) list) (board : Board.board)
    (color : Board.piece) : int * int =
  if List.length valid_moves = 0 then
    raise (Invalid_argument "error, possible move list is 0")
  else
    let movePos = Random.int (List.length valid_moves) in
    generateMoveHelper valid_moves movePos 0

let count_flipped_pieces original_board new_board color =
  let original_count = Board.count_pieces original_board color in
  let new_count = Board.count_pieces new_board color in
  new_count - original_count

let generateMoveMedium (valid_moves : (int * int) list) (board : Board.board)
    (color : Board.piece) : int * int =
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

let rec alpha_beta_prune board depth alpha beta color maximizingPlayer =
  if depth = 0 || Board.is_board_filled board then
    (evaluate_board board color, (-1, -1))
  else
    let valid_moves = Board.find_all_valid_moves color board in
    if List.length valid_moves = 0 then (evaluate_board board color, (-1, -1))
    else
      let best_value =
        ref (if maximizingPlayer then Int.min_int else Int.max_int)
      in
      let best_move = ref (-1, -1) in
      List.iter
        (fun (x, y) ->
          let new_board = Board.place_and_flip_pieces x y color board in
          let eval, _ =
            alpha_beta_prune new_board (depth - 1) alpha beta
              (match color with
              | White -> Black
              | Black -> White
              | Empty -> failwith "bruh")
              (not maximizingPlayer)
          in
          if
            (maximizingPlayer && eval > !best_value)
            || ((not maximizingPlayer) && eval < !best_value)
          then (
            best_value := eval;
            best_move := (x, y));
          if maximizingPlayer then alpha := max !alpha eval
          else beta := min !beta eval;
          if !beta <= !alpha then ())
        valid_moves;
      (!best_value, !best_move)

(** Generate move for god mode AI using Minimax w/ Alpha-Beta Pruning *)
let generateMoveExtreme (valid_moves : (int * int) list) board color =
  let depth = 5 in
  (*Increase/decrease the depth to increase/decrease difficulty*)
  let _, best_move =
    alpha_beta_prune board depth (ref Int.min_int) (ref Int.max_int) color true
  in
  best_move
