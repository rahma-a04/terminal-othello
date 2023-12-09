open Othello

(** Parses a move input on a given board into tuple form. Requires: [move] must
    be in the form "a b", where a is the row and b is the column of the intended
    move. Returns None if the move is invalid. *)
let evaluate_move (move : string) board color : (int * int) option =
  let row = int_of_string (String.sub move 0 1) in
  let col = int_of_string (String.sub move 2 1) in
  if Board.is_legit board row col color then Some (row, col) else None
(*TODO: this impl sucks, figure out a better way*)
(*TODO: this impl isn't defensive enough; need to catch invalid argument errors*)

(** Ends the game. Displays the final scores and the winner on the terminal
    window. *)
let end_game board =
  let black_score = Board.count_pieces board Board.Black in
  (*todo: it would prob be better to return a tuple in count_pieces*)
  let white_score = Board.count_pieces board Board.White in
  let winner =
    if black_score > white_score then "Black"
    else if black_score = white_score then "Tie"
    else "White"
  in
  print_endline
    ("Game over!\nBlack score: " ^ string_of_int black_score ^ "\nWhite score: "
   ^ string_of_int white_score ^ "\nWINNER: " ^ winner)

(** Main gameplay function. Allows players to input moves and place pieces in
    alternating order until board is filled. *)

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

let pp_int (x, y) = "\"" ^ string_of_int x ^ ", " ^ string_of_int y ^ "\""

let rec play_game board (is_black : bool) =
  Board.print_board board;
  (* check whether game is over *)
  if Board.is_board_filled board then end_game board
  else
    let player, piece =
      if is_black then ("Black", Board.Black) else ("White", Board.White)
    in
    (* Checks to see if there are moves left for the players*)
    let potentialPlayerMoves = Board.find_all_valid_moves piece board in
    if potentialPlayerMoves = [] then
      let () = print_endline "No more moves left" in
      play_game board (not is_black)
    else
      (*TODO: this is dumb*)
      print_string
        ("[Player: " ^ player
       ^ "]\nEnter move as 'row col' (i.e. 3 2)\nType 'q' to quit\n "
       ^ "Possible Moves: "
        ^ pp_list pp_int (Board.find_all_valid_moves piece board)
        ^ "\n>");

    let response = String.trim (read_line ()) in
    match response with
    | "q" -> print_endline "goodbye!"
    | _ -> (
        let row_col =
          evaluate_move response board (if is_black then Black else White)
        in
        match row_col with
        | Some (row, col) ->
            print_endline "\n\n\n";
            play_game (Board.place_piece row col piece board) (not is_black)
        | None ->
            print_endline "\n\n\n***Invalid move! Try again.***";
            play_game board is_black)

let rec play_ComputerPlayer board (is_black : bool)
    (computerColorisBlack : bool) =
  Board.print_board board;

  (* check whether game is over *)
  if Board.is_board_filled board then end_game board
  else
    let player, piece =
      if is_black then ("Black", Board.Black) else ("White", Board.White)
    in
    let computerPlayer, computerPiece =
      if computerColorisBlack then ("Black", Board.Black)
      else ("White", Board.White)
    in

    let potenitalComputerMoves =
      Board.find_all_valid_moves computerPiece board
    in
    let potentialPlayerMoves = Board.find_all_valid_moves piece board in

    (* Checks to see if there are moves left for the players*)
    if potentialPlayerMoves = [] && potenitalComputerMoves = [] then
      end_game board
    else if potenitalComputerMoves = [] || potentialPlayerMoves = [] then
      let () = print_endline "No more moves left" in
      play_ComputerPlayer board (not is_black) computerColorisBlack
    else if player = computerPlayer then (
      let moves = Board.find_all_valid_moves computerPiece board in
      let response = ComputerPlayer.generateMove moves in
      match response with
      | row, col ->
          print_endline "\n\n\n";
          print_endline
            ("Interesting move, let me think!  : " ^ string_of_int row ^ ", "
           ^ string_of_int col);
          play_ComputerPlayer
            (Board.place_and_flip_pieces row col computerPiece board)
            (not is_black) computerColorisBlack)
    else (
      print_string
        ("[Player: " ^ player
       ^ "]\nEnter move as 'row col' (i.e. 3 2)\nType 'q' to quit\n "
       ^ "Possible Moves: "
        ^ pp_list pp_int (Board.find_all_valid_moves piece board)
        ^ "\n>");

      let response = String.trim (read_line ()) in
      match response with
      | "q" -> print_endline "goodbye!"
      | _ -> (
          let row_col = evaluate_move response board piece in
          match row_col with
          | Some (row, col) ->
              print_endline "\n\n\n";
              play_ComputerPlayer
                (Board.place_and_flip_pieces row col piece board)
                (not is_black) computerColorisBlack
          | None ->
              print_endline "\n\n\n***Invalid move! Try again.***";
              play_ComputerPlayer board (not is_black) computerColorisBlack))

let () =
  print_endline
    "Welcome to Othello! Would you like to start a computer game or a two \
     player game? You can type n to exit. Reponses (computer player, two \
     player,n)";
  print_string "> ";
  let response = read_line () in
  match String.trim response with
  | "computer player" -> (
      print_endline "Welcome to Othello!";
      print_endline
        "Would you like to be black or white? Responses (black/white)";
      let next_response = read_line () in
      match next_response with
      | "black" -> play_ComputerPlayer Board.empty_board true false
      | "white" -> play_ComputerPlayer Board.empty_board false true
      | _ ->
          print_endline "invalid response!";
          play_ComputerPlayer Board.empty_board true true)
  | "two player" ->
      print_endline "Welcome to Othello!";
      play_game Board.empty_board
        true (*note: change it so that it's case-insensitive*)
  | "n" -> print_endline ("Goodbye!" ^ "\n" ^ "Come back another time :)")
  | _ -> print_endline "Invalid response"
