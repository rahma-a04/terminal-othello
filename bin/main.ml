open Othello

let evaluate_move (move : string) : int * int =
  let row = int_of_string (String.sub move 0 1) in 
  let col = int_of_string (String.sub move 2 1) in (*TODO: this impl sucks, figure out a better way*)
  (row, col) (*TODO: this impl isn't defensive enough; need to catch invalid argument errors*)

let rec play_game board (is_black : bool) =
  Board.print_board board;
  let player, piece = if is_black then "Black", Board.Black else "White", Board.White in (*TODO: this is dumb*)
  print_string ("[Player: " ^ player ^ "]\nEnter move as 'row col' (i.e. 3 2)\nType 'q' to quit\n> ");
  let response = String.trim (read_line ()) in 
  match response with
  | "q" -> print_endline("goodbye!")
  | _ -> let row_col = evaluate_move response in 
  play_game (Board.place_piece (fst row_col) (snd row_col) piece board) (not is_black) 

let () = 
  print_endline "Welcome to Othello! Start game? (y/n)";
  print_string "> ";
  let response = read_line () in
  match String.trim response with
  | "y" -> print_endline("Welcome to Othello!"); play_game Board.empty_board true(*note: change it so that it's case-insensitive*)
  | "n" -> print_endline("goodbye!")
  | _ -> print_endline("Invalid response")