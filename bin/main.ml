open Othello
open Ui

type difficulty =
  | Easy
  | Medium
  | Hard

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

let column_map =
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

let eval_move (str : string) (game : game) =
  match
    str |> String.trim |> String.lowercase_ascii |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  with
  | [ col_letter; row_one ] -> begin
      try
        update
          (int_of_string row_one - 1) (*could fail with Failure*)
          (List.assoc col_letter column_map) (*could fail with Not_found*)
          game
      with Not_found | Failure _ ->
        raise (Invalid_argument "Could not parse input")
    end
  | _ -> raise (Invalid_argument "Could not parse input")

let init_msg =
  "Welcome to Othello!\n\
   Type 's' for singleplayer, 'm' for multiplayer, 'q' to quit, and 'h' for \
   help."

let invalid_msg = "Invalid response! Type 'h' for valid commands"

let init_help_commands =
  "COMMANDS:\n\n\
   \'s easy': enters a singleplayer game against an easy difficulty AI opponent\n\
   's medium': enters a singleplayer game against a medium difficulty AI \
   opponent\n\
   's hard': enters a singleplayer game against a hard difficulty AI opponent\n\
   'm': enters a multiplayer game\n\
   'h': displays commands\n\
   'q': quits game"

let multi_game_commands =
  "COMMANDS:\n\n\n\
  \  To enter a move, please type 'column row' (i.e. F 6)\n\n\
  \  'h': displays commands\n\n\
  \  'q': quits game"

let default_main_msg = "Enter move (e.g. F 5)"

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
  | [ "m" ] -> play (Main (Multi, Ui.new_game))
  | [ "q" ] -> print_endline "Goodbye!"
  | [ "h" ] -> initialize init_help_commands
  | _ -> initialize invalid_msg

and multi (msg : string) (game : game) =
  print_current_game game;
  print_endline msg;
  print_endline
    ("VALID MOVES: "
    ^ (Board.find_all_valid_moves (player_of_game game) (board_of_game game)
      |> pp_list (fun (x, y) -> string_of_int x ^ " " ^ string_of_int y)));
  print_string "> ";
  let resp = read_line () |> String.trim |> String.lowercase_ascii in
  match resp with
  | "h" -> multi multi_game_commands game
  | "q" -> print_endline "Goodbye!"
  | _ -> (
      try
        let updated_game = eval_move resp game in
        multi default_main_msg updated_game
      with Invalid_argument e -> multi (invalid_msg ^ "\nERROR: " ^ e) game)

and play (state : state) =
  match state with
  | Initialize -> initialize init_msg
  | Main (mode, game) -> (
      match mode with
      | Multi -> multi default_main_msg game
      | Single difficulty -> failwith "U")
  | History game -> failwith "U"
  | End game -> failwith "U"

(* parse input for move multiplayer: check if move is valid -> if yes, place
   piece and update board -> if no, retry for move singleplayer: check if move
   is valid -> if yes, place piece and update board -> pass to AI -> place AI
   piece and update board for history: print history -> switch to a new game
   mode -> check if represents a valid location in history for help: print help
   commands*)

let () = play Initialize
