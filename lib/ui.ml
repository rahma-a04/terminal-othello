open Board

exception End_game

type game_state = {
  board : board;
  move_num : int;
  player : piece;
}

type game = game_state list

let new_game =
  let state = { board = empty_board; move_num = 0; player = Black } in
  [ state ]

(** Prints the player as Black or White*)
let print_player p =
  match p with
  | Black -> "Black"
  | White -> "White"
  | _ -> failwith "Impossible"

let board_of_game g =
  match g with
  | { board = b; move_num = _; player = _ } :: t -> b
  | [] -> failwith "Invalid input: game doesn't exist"

let player_of_game g =
  match g with
  | { board = _; move_num = _; player = p } :: t -> p
  | [] -> failwith "Invalid input: game doesn't exist"

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

let revert (game : game) n =
  if n < 0 || n > List.length game - 1 then
    raise
      (Failure (string_of_int n ^ " is outside the game's valid move range."))
  else trim (List.length game - (n + 1)) game

(** Prints a game state *)
let print_curr_state (state : game_state) =
  match state with
  | { board = b; move_num = n; player = p } ->
      let code = if p = Black then black_circle_code else white_circle_code in
      print_endline ("[MOVE NUMBER: " ^ string_of_int n ^ "]");
      print_board b;
      print_endline ("[PLAYER: " ^ code ^ " " ^ print_player p ^ "]")

let print_current_game (game : game) =
  match game with
  | h :: t -> print_curr_state h
  | [] -> failwith "Impossible"

(** Helper for print_previous_games *)
let rec print_prev_helper = function
  | h :: t ->
      print_curr_state h;
      print_newline ();
      print_endline
        "*====================================================================*";
      print_newline ();
      print_prev_helper t
  | [] -> print_newline ()

(** Prints the game history, including all moves that were made in the game. *)
let print_previous_games (game : game) =
  print_newline ();
  print_endline
    "*========================== GAME HISTORY ==========================*\n";
  print_prev_helper (List.rev game)
