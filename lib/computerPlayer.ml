(** Randomly selects possible move from all possible moves *)
let rec generateMoveHelper (valid_moves : (int * int) list) (v : int)
    (currentNum : int) : int * int =
  match valid_moves with
  | (x, y) :: tail ->
      if currentNum <> v then generateMoveHelper tail v (currentNum + 1)
      else (x, y)
  | [] -> raise (Invalid_argument "no more moves :(")

(** Uses generateMoveHelper to select a random move out of all possible moves *)
let generateMoveEasy (valid_moves : (int * int) list) (board : Board.board)
    (color : Board.piece) : int * int =
  if List.length valid_moves = 0 then
    raise (Invalid_argument "error, possible move list is 0")
  else
    let movePos = Random.int (List.length valid_moves) in
    generateMoveHelper valid_moves movePos 0

(**Counts the number of pieces flipped*)
let count_flipped_pieces original_board new_board color =
  let original_count = Board.count_pieces original_board color in
  let new_count = Board.count_pieces new_board color in
  new_count - original_count

(** Selects the move that captures the most number of pieces out of out of all
    possible moves *)
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

(** Evaluates the weight of a specific move (x, y) based on a predefined weight
    matrix. This function returns the weight score of the move for a given
    color. *)
let evaluate_move (x, y) color =
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
  weight_matrix.(x).(y)

(** Selects the best move based on preset weight matrix, and if a tie occurs,
    the algorithm takes preference to the move that captures the most number of
    pieces *)
let generateMoveHard (valid_moves : (int * int) list) (board : Board.board)
    (color : Board.piece) : int * int =
  let simulate_move (x, y) =
    let move_score = evaluate_move (x, y) color in
    let simulated_board = Board.place_and_flip_pieces x y color board in
    let flipped_count = count_flipped_pieces board simulated_board color in
    (move_score, flipped_count, (x, y))
  in
  let moves_with_scores = List.map simulate_move valid_moves in
  let compare (score1, flips1, _) (score2, flips2, _) =
    let score_comp = compare score2 score1 in
    if score_comp = 0 then compare flips2 flips1 else score_comp
  in
  match List.sort compare moves_with_scores with
  | [] -> raise (Invalid_argument "no more moves :(")
  | (_, _, best_move) :: _ -> best_move

(** Evaluates the move (x, y) for the godmode, combining the static weight
    matrix evaluation with dynamic game factors. It considers both the move's
    strategic importance and the number of opponent pieces that would be
    flipped. Additional consideration is given for controlling edges and
    corners. Returns a tuple of the move's weight and the count of flipped
    pieces. *)
let evaluate_move_extreme (x, y) color board =
  let weight_matrix =
    [|
      [| 5; -3; 3; 2; 2; 3; -3; 5 |];
      [| -3; -4; -1; -1; -1; -1; -4; -3 |];
      [| 3; -1; 1; 0; 0; 1; -1; 3 |];
      [| 2; -1; 0; 1; 1; 0; -1; 2 |];
      [| 2; -1; 0; 1; 1; 0; -1; 2 |];
      [| 3; -1; 1; 0; 0; 1; -1; 3 |];
      [| -3; -4; -1; -1; -1; -1; -4; -3 |];
      [| 5; -3; 3; 2; 2; 3; -3; 5 |];
    |]
  in
  let move_weight = if x >= 0 && y >= 0 then weight_matrix.(x).(y) else 0 in
  let simulated_board = Board.place_and_flip_pieces x y color board in
  let flipped_count = count_flipped_pieces board simulated_board color in
  let is_edge_or_corner (x, y) =
    (x = 0 || x = 7 || y = 0 || y = 7)
    || (x = 0 && y = 0)
    || (x = 7 && y = 7)
    || (x = 0 && y = 7)
    || (x = 7 && y = 0)
  in
  let additional_edge_bonus =
    if is_edge_or_corner (x, y) && flipped_count > 0 then 2 else 0
  in
  (move_weight + additional_edge_bonus, flipped_count)

(** Implements the Minimax algorithm with Alpha-Beta pruning to optimize the
    AI's move selection. It evaluates moves recursively up to a given depth,
    pruning branches that don't need to be explored. The function takes the
    current board, search depth, alpha, beta, current player's color, and a
    boolean indicating if the current player is maximizing or minimizing. It
    returns the best move found at that depth. *)
let rec alpha_beta_prune board depth alpha beta color maximizingPlayer =
  if depth = 0 || Board.is_board_filled board then
    evaluate_move_extreme (-1, -1) color board
  else
    let valid_moves = Board.find_all_valid_moves color board in
    if valid_moves = [] then evaluate_move_extreme (-1, -1) color board
    else
      let best_value =
        ref (if maximizingPlayer then Int.min_int else Int.max_int)
      in
      let best_move = ref (-1, -1) in
      List.iter
        (fun (x, y) ->
          let new_board = Board.place_and_flip_pieces x y color board in
          let eval, flipped_count =
            evaluate_move_extreme (x, y) color new_board
          in
          let total_score = eval + flipped_count in
          (* Combining two scoring criteria, weight matrix & flip count*)
          if
            (maximizingPlayer && total_score > !best_value)
            || ((not maximizingPlayer) && total_score < !best_value)
          then (
            best_value := total_score;
            best_move := (x, y));
          if maximizingPlayer then alpha := max !alpha total_score
          else beta := min !beta total_score;
          if !beta <= !alpha then ())
        valid_moves;
      !best_move

(** Generates a move for the extreme AI level. This function applies the
    alpha-beta pruning algorithm & evaluate move extreme to evaluate moves
    several steps ahead, *)
let generateMoveExtreme (valid_moves : (int * int) list) board color =
  let depth = 5 (* Adjust depth for difficulty *) in
  let x, y =
    alpha_beta_prune board depth (ref Int.min_int) (ref Int.max_int) color true
  in
  (x, y)
