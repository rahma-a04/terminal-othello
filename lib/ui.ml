open Board

exception End_game

(* Describes the type game state, which is composed of a board, the particular
   move, and a player*)
type game_state = {
  board : board;
  move_num : int;
  player : piece;
}

(* Defines type game to be a list of game_state*)
type game = game_state list

(* Creates a new game *)
let new_game =
  let state = { board = empty_board; move_num = 0; player = Black } in
  [ state ]

(* Prints the player as Black or White*)
let print_player p =
  match p with
  | Black -> "Black"
  | White -> "White"
  | _ -> failwith "Impossible"

(*Matches the input with a game and returns the board. Raises: invalid_argument
  if the input is not a game*)
let board_of_game g =
  match g with
  | { board = b; move_num = _; player = _ } :: t -> b
  | [] -> failwith "Invalid input: game doesn't exist"

(* Matches the input with a game and returns the player. Raises:
   invalid_argument if the input is not a game*)
let player_of_game g =
  match g with
  | { board = _; move_num = _; player = p } :: t -> p
  | [] -> failwith "Invalid input: game doesn't exist"

(** Updates a game by placing a piece of the game's current color at (row, col)
    in the game's board. Raises: invalid_argument if the move is invalid.
    Raises: End_game if game cannot be updated *)
let update (row : int) (col : int) (game : game) : game =
  match game with
  | ({ board = b; move_num = n; player = p } as h) :: t -> begin
      let new_player = if p = White then Black else White in
      match
        ( find_all_valid_moves p b |> List.length = 0,
          find_all_valid_moves new_player b |> List.length = 0 )
      with
      | true, true -> raise End_game
      | true, false ->
          { board = b; move_num = n; player = new_player } :: h :: t
      | _ ->
          if is_legit b row col p then
            let new_board = place_and_flip_pieces row col p b in
            { board = new_board; move_num = n + 1; player = new_player }
            :: h :: t
          else raise (Invalid_argument "Not a valid move")
    end
  | [] -> failwith "Invalid input: game doesn't exist"

(** Skips the current player's turn in the game. Raises: invalid_argument if a
    game is not passed in*)
let skip_turn (game : game) =
  match game with
  | { board = b; move_num = n; player = p } :: t ->
      let new_player = if p = White then Black else White in
      { board = b; move_num = n; player = new_player } :: t
  | [] -> failwith "Invalid input: game doesn't exist"

(** Trims x elements off the beginning of a list *)
let rec trim x lst =
  if x = 0 then lst
  else
    match lst with
    | h :: t -> trim (x - 1) t
    | [] -> []

(* Moves a game back to a previous game state *)
let revert (game : game) n =
  if n < 0 || n > List.length game - 1 then
    raise
      (Failure (string_of_int n ^ " is outside the game's valid move range."))
  else trim (List.length game - (n + 1)) game

(* Prints the current game state *)
let print_curr_state (state : game_state) =
  match state with
  | { board = b; move_num = n; player = p } ->
      let code = if p = Black then black_circle_code else white_circle_code in
      print_endline ("[MOVE NUMBER: " ^ string_of_int n ^ "]");
      print_board b;
      print_endline ("[PLAYER: " ^ code ^ " " ^ print_player p ^ "]")

(* Prints every state of a game to print a game*)
let print_current_game (game : game) =
  match game with
  | h :: t -> print_curr_state h
  | [] -> failwith "Impossible"

(* A helper function to print_previous_games that prints every state of a game*)
let rec print_prev_helper = function
  | h :: t ->
      print_curr_state h;
      print_newline ();
      print_endline
        "*====================================================================*";
      print_newline ();
      print_prev_helper t
  | [] -> print_newline ()

(* A function that prints the game history and calls print_prev_helper *)
let print_previous_games (game : game) =
  print_newline ();
  print_endline
    "*========================== GAME HISTORY ==========================*\n";
  print_prev_helper (List.rev game)
