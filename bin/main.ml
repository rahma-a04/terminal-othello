open Othello
open Ui

type difficulty =
  | Easy
  | Medium
  | Hard
  | Extreme

type gamemode =
  | Single of difficulty
  | Multi

type state =
  | Initialize
  | Main of (gamemode * game)
  | History of game
  | End of game

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let letters_to_ints =
  [
    ("a", 0);
    ("b", 1);
    ("c", 2);
    ("d", 3);
    ("e", 4);
    ("f", 5);
    ("g", 6);
    ("h", 7);
  ]

let computerResponseList =
  [
    "I'm coming for your job";
    "You thought you ate :| ";
    "I didn't know we were playing checkers. Oh wait, we're not!";
    "Do you need a GPS to find a winning move?";
    "I didn't realize we were playing hide-and-seek with your winning moves.";
    "Are you trying to set a record for the longest losing streak?";
    "Are you playing Othello or just arranging the pieces for modern art?";
    "Are you giving me a head start, or is this your actual strategy?";
    "Your moves are so weak, even the pieces are yawning.";
    "I've heard of charity, but you are practically donating pieces";
    "I hope your day job doesn't involve decision-making.";
  ]

let ints_to_letters =
  [
    (0, "a");
    (1, "b");
    (2, "c");
    (3, "d");
    (4, "e");
    (5, "f");
    (6, "g");
    (7, "h");
  ]

let init_msg =
  "Welcome to Othello!\n\
   Type 's [easy | medium | hard | godmode]' for singleplayer, 'm' for \
   multiplayer, 'q' to quit, and 'h' for help."

let invalid_msg = "Invalid response! Type 'h' for valid commands"

let init_help_commands =
  "COMMANDS:\n\n\
   \'s easy': enters a singleplayer game against an easy difficulty AI opponent\n\
   's medium': enters a singleplayer game against a medium difficulty AI \
   opponent\n\
   's hard': enters a singleplayer game against a hard difficulty AI opponent\n\
   's godmode': enters a singleplayer game against a extremely difficult AI \
   opponent\n\
   'm': enters a multiplayer game\n\
   'h': displays commands\n\
   'q': quits game"

let game_commands =
  "COMMANDS:\n\n\n\
  \  To enter a move, please type 'column row' (i.e. F 6)\n\n\
  \  'h': displays commands\n\n\
  \  'q': quits game\n\n\
  \  'history': enters history mode"

let default_main_msg = "Enter move (e.g. F 5). Type 'h' for valid commands."
let single_choose_color = "Choose 'b' to play as Black, 'w' to play as White"

let history_welcome_msg =
  "Welcome to history mode! To view all past moves in this game and select a \
   move to return to, type 'show'. Type 'exit' to return to the current state \
   of the game."

let history_selector_msg = "Type a move number to return to (e.g. '2')."

let history_help_msg_a =
  "COMMANDS:\n\n\n\
  \ 'show': shows current game's history and allows you to select a state to \
   return to\n\n\
  \ 'exit': exits history mode\n\n\
  \ 'h': displays commands\n\n\
  \ 'q': quits othello"

let history_help_msg_b =
  "COMMANDS:\n\n\n\
  \ Type the move # of a point in the printed history to return to that point \
   in the game!\n\
  \ (e.g. '2' will take you back to [MOVE NUMBER: 2])\n\n\
  \ 'exit': exits history mode\n\n\
  \ 'h': displays commands\n\n\
  \ 'q': quits othello"

let history_show_msg =
  "Please select a move number in your current game to return to (e.g. '2').\n\
  \  \n\
  \   WARNING: Once selected, you will not be able to restore any progress \
   after that point! \n\
   Type 'h' for help, 'exit' to return to your current game, and 'q' to quit \
   Othello."

let closing_top =
  "\u{2554}\u{2550}.\u{2735}.\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2557}"

let closing_bottom =
  "\u{255A}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}\u{2550}.\u{2735}.\u{2550}\u{255D}"

let is_string_int (s : string) =
  try (true, int_of_string s) with Failure _ -> (false, -1)

let eval_move (str : string) (game : game) =
  match
    str |> String.trim |> String.lowercase_ascii |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  with
  | [ col_letter; row_one ] -> begin
      try
        update
          (int_of_string row_one - 1) (*could fail with Failure*)
          (List.assoc col_letter letters_to_ints) (*could fail with Not_found*)
          game
      with Not_found | Failure _ ->
        raise (Invalid_argument "Could not parse input")
    end
  | _ -> raise (Invalid_argument "Could not parse input")

let rec initialize (msg : string) =
  print_endline msg;
  print_string "> ";
  let response =
    read_line () |> String.trim |> String.lowercase_ascii
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  match response with
  | [ "s"; "easy" ] -> play (Main (Single Easy, Ui.new_game))
  | [ "s"; "medium" ] -> play (Main (Single Medium, Ui.new_game))
  | [ "s"; "hard" ] -> play (Main (Single Hard, Ui.new_game))
  | [ "s"; "godmode" ] -> play (Main (Single Extreme, Ui.new_game))
  | [ "m" ] -> play (Main (Multi, Ui.new_game))
  | [ "q" ] -> print_endline "Goodbye!"
  | [ "h" ] -> initialize init_help_commands
  | _ -> initialize invalid_msg

and history_show (msg : string) (game : game) (gamemode : gamemode)
    (human_player : Board.piece) (print_games : bool) =
  if print_games then (
    print_previous_games game;
    print_endline "GAME HISTORY ABOVE")
  else ();
  print_endline msg;
  print_string "> ";
  let resp = read_line () |> String.trim |> String.lowercase_ascii in
  match resp with
  | "exit" -> (
      match gamemode with
      | Multi -> multi ("Welcome back! " ^ default_main_msg) game
      | Single d ->
          single ("Welcome back! " ^ default_main_msg) d game human_player)
  | "q" -> print_endline "Goodbye!"
  | "h" -> history_show history_help_msg_b game gamemode human_player false
  | s -> begin
      match is_string_int s with
      | true, i -> (
          try
            let reverted_game = revert game i in
            print_endline ("Reverting game to MOVE NUMBER: " ^ string_of_int i);
            match gamemode with
            | Multi -> multi ("Welcome back! " ^ default_main_msg) reverted_game
            | Single d ->
                single
                  ("Welcome back! " ^ default_main_msg)
                  d reverted_game human_player
          with Failure f ->
            history_show
              (invalid_msg ^ "\nERROR: " ^ f)
              game gamemode human_player false)
      | false, _ -> history_show invalid_msg game gamemode human_player false
    end

and history (msg : string) (game : game) (gamemode : gamemode)
    (human_player : Board.piece) =
  print_newline ();
  print_endline msg;
  print_string "> ";
  let resp = read_line () |> String.trim |> String.lowercase_ascii in
  match resp with
  | "show" -> history_show history_show_msg game gamemode human_player true
  | "exit" -> (
      match gamemode with
      | Multi -> multi ("Welcome back! " ^ default_main_msg) game
      | Single d ->
          single ("Welcome back! " ^ default_main_msg) d game human_player)
  | "q" -> print_endline "Goodbye!"
  | "h" -> history history_help_msg_a game gamemode human_player
  | _ -> history invalid_msg game gamemode human_player

and multi (msg : string) (game : game) =
  print_newline ();
  print_current_game game;
  print_endline msg;
  print_endline
    ("VALID MOVES: "
    ^ (Board.find_all_valid_moves (player_of_game game) (board_of_game game)
      |> pp_list (fun (x, y) ->
             List.assoc x ints_to_letters ^ " " ^ string_of_int (y + 1))));
  print_string "> ";
  let resp = read_line () |> String.trim |> String.lowercase_ascii in
  match resp with
  | "h" -> multi game_commands game
  | "q" -> print_endline "Goodbye!"
  | "history" -> history history_welcome_msg game Multi Black
  | _ -> begin
      try
        if Board.is_board_filled (board_of_game game) then play (End game)
        else
          let updated_game = eval_move resp game in
          multi default_main_msg updated_game
      with
      | Invalid_argument e -> multi (invalid_msg ^ "\nERROR: " ^ e) game
      | End_game -> play (End game)
    end

(* Computer player logic called by main *)
and single (msg : string) (mode : difficulty) (game : game)
    (human_player : Board.piece) =
  (* print the current game state *)
  print_endline "";
  print_current_game game;
  (* check who is playing the game *)
  let generate_move =
    match mode with
    | Easy -> ComputerPlayer.generateMoveEasy
    | Medium -> ComputerPlayer.generateMoveMedium
    | Hard -> ComputerPlayer.generateMoveHard
    | Extreme -> ComputerPlayer.generateMoveExtreme
  in
  let otherPlayer =
    match player_of_game game with
    | Black -> Board.White
    | White -> Board.Black
    | Empty -> failwith "Something weird is happening"
  in
  if otherPlayer != human_player then begin
    print_endline msg;
    let valid_moves_list =
      Board.find_all_valid_moves (player_of_game game) (board_of_game game)
    in
    let otherPlayerMoves =
      Board.find_all_valid_moves otherPlayer (board_of_game game)
    in
    match (List.length otherPlayerMoves, List.length valid_moves_list) with
    | 0, 0 -> play (End game)
    | _, 0 ->
        single "No more valid moves! Skipping your turn..." mode
          (skip_turn game) human_player
    | _, _ -> (
        print_endline
          ("VALID MOVES: "
          ^ (valid_moves_list
            |> pp_list (fun (x, y) ->
                   List.assoc x ints_to_letters ^ " " ^ string_of_int (y + 1)))
          );
        print_string "> ";
        let resp = read_line () |> String.trim |> String.lowercase_ascii in
        match resp with
        | "h" -> single game_commands mode game human_player
        | "q" -> print_string "Goodbye!"
        | "history" ->
            history history_welcome_msg game (Single mode) human_player
        | _ -> (
            try
              let pos = Random.int (List.length computerResponseList) in
              print_endline (List.nth computerResponseList pos);
              if Board.is_board_filled (board_of_game game) then play (End game)
              else
                single default_main_msg mode (eval_move resp game) human_player
            with
            | Invalid_argument e ->
                single (invalid_msg ^ "\nERROR: " ^ e) mode game human_player
            | End_game -> play (End game)))
  end
  else begin
    let valid_moves_list =
      Board.find_all_valid_moves (player_of_game game) (board_of_game game)
    in
    let otherPlayerMoves =
      Board.find_all_valid_moves otherPlayer (board_of_game game)
    in
    match (List.length otherPlayerMoves, List.length valid_moves_list) with
    | 0, 0 -> play (End game)
    | _, 0 ->
        single "No more valid moves! Skipping your turn..." mode
          (skip_turn game) human_player
    | _, _ -> (
        let move =
          generate_move
            (Board.find_all_valid_moves (player_of_game game)
               (board_of_game game))
            (board_of_game game) (player_of_game game)
        in
        try
          print_endline
            ("Computer move: "
            ^ string_of_int (fst move)
            ^ " "
            ^ string_of_int (snd move));
          print_endline
            "*-----------------------------------------------------*";
          single default_main_msg mode
            (update (snd move) (fst move) game)
            human_player
        with End_game -> play (End game))
  end

and end_game (game : game) =
  let board = board_of_game game in
  let black_score = Board.count_pieces board Board.Black in
  (*todo: it would prob be better to return a tuple in count_pieces*)
  let white_score = Board.count_pieces board Board.White in
  let winner =
    if black_score > white_score then "Black"
    else if black_score = white_score then "Tie!"
    else "White"
  in
  print_endline closing_top;
  print_endline
    ("     Game over!\n   Black score: " ^ string_of_int black_score
   ^ "\n   White score: " ^ string_of_int white_score ^ "\n   WINNER: " ^ winner
    );
  print_endline "      Goodbye!";
  print_endline closing_bottom

and play (state : state) =
  match state with
  | Initialize -> initialize init_msg
  | Main (mode, game) -> (
      match mode with
      | Multi -> multi default_main_msg game
      | Single difficulty -> (
          print_endline single_choose_color;
          print_string "> ";
          let resp = read_line () |> String.trim |> String.lowercase_ascii in
          match resp with
          | "b" -> single default_main_msg difficulty game Black
          | "w" -> single default_main_msg difficulty game White
          | _ ->
              print_endline "Please choose a valid color!";
              play (Main (Single difficulty, game))))
  | History game -> failwith "Something went wrong"
  | End game -> end_game game

(* parse input for move multiplayer: check if move is valid -> if yes, place
   piece and update board -> if no, retry for move singleplayer: check if move
   is valid -> if yes, place piece and update board -> pass to AI -> place AI
   piece and update board for history: print history -> switch to a new game
   mode -> check if represents a valid location in history for help: print help
   commands*)

let () = play Initialize
