(** Creates the board of Othello, and provide a function that prints it out in
    terminal. *)

(** Represents Othello game pieces: Black, White, or Empty. *)
type piece =
  | Black
  | White
  | Empty

type board = piece list list
(** Represents the Othello game board as a list of lists of pieces. *)

(** The starting position of Othello. *)
let empty_board =
  [
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
    [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
  ]

let black_circle_code = "\u{25CB}"
let white_circle_code = "\u{25CF}"
let empty_code = " "

(** Converts a list of pieces to a list of string representations. *)
let rec to_list_small lst =
  match lst with
  | [] -> []
  | h :: t ->
      if h = Empty then empty_code :: to_list_small t
      else if h = Black then black_circle_code :: to_list_small t
      else white_circle_code :: to_list_small t

(** Converts the game board to a list of string representations. *)
let rec to_list board =
  match board with
  | [] -> []
  | h :: t -> to_list_small h :: to_list t

let to_piece_list board = board

(** Prints a single list with "|" in between each element. *)
let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_string h;
      print_string " | ";
      print_list t

(** Prints the first "|" of the board. *)
let board_row (lst : string list) : unit =
  print_string "| ";
  print_list lst

(** Prints a list of lists with a line in between each. *)
let rec print_list_of_lists n = function
  | [] -> ()
  | h :: t ->
      board_row h;
      print_string (string_of_int n);
      print_newline ();
      print_string "---------------------------------";
      print_newline ();
      print_list_of_lists (n + 1) t

(** Prints the top of the board, including letter labels. *)
let print_board_top (lst : string list list) : unit =
  print_string "  A   B   C   D   E   F   G   H";
  print_newline ();
  print_string "---------------------------------";
  print_newline ();
  print_list_of_lists 1 lst

(** Prints the entire game board. *)
let print_board (board : piece list list) : unit =
  print_board_top (to_list board)

(** Counts the number of a specific piece in a list. *)
let rec count_number_of_objs_in_list lst (obj : piece) =
  match lst with
  | [] -> 0
  | h :: t ->
      if h = obj then 1 + count_number_of_objs_in_list t obj
      else count_number_of_objs_in_list t obj

(** Counts the total number of a specific piece on the board. *)
let rec count_pieces (board : piece list list) (color : piece) =
  match board with
  | [] -> 0
  | h :: t -> count_number_of_objs_in_list h color + count_pieces t color

(** Checks if the board is completely filled with pieces.*)
let is_board_filled (board : piece list list) =
  if count_pieces board Empty > 0 then false else true

(** Retrieves the piece at a specific row and column on the board. *)
let get_element row col board =
  try
    let selected_row = List.nth board row in
    List.nth selected_row col
  with Failure _ -> failwith "Invalid row or column index"

(** Updates the spot with an element. *)
let rec update_element row col value board =
  List.mapi
    (fun i r ->
      if i = row then List.mapi (fun j v -> if j = col then value else v) r
      else r)
    board

(** Places a piece at a specified location on the board. *)
let place_piece x y (color : piece) board = update_element x y color board

(** Retrieves all positions northeast of a given point. *)
let rec northeast curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: northeast (curr_x + 1) (curr_y + 1) board

(** Retrieves all positions east of a given point. *)
let rec east curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: east (curr_x + 1) curr_y board

(** Retrieves all positions southeast of a given point. *)
let rec southeast curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: southeast (curr_x + 1) (curr_y - 1) board

(** Retrieves all positions south of a given point. *)
let rec south curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: south curr_x (curr_y - 1) board

(** Retrieves all positions southwest of a given point. *)
let rec southwest curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: southwest (curr_x - 1) (curr_y - 1) board

(** Retrieves all positions west of a given point. *)
let rec west curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: west (curr_x - 1) curr_y board

(** Retrieves all positions northwest of a given point. *)
let rec northwest curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: northwest (curr_x - 1) (curr_y + 1) board

(** Retrieves all positions north of a given point. *)
let rec north curr_x curr_y board =
  if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
  else
    match get_element curr_x curr_y board with
    | Empty -> []
    | _ -> (curr_x, curr_y) :: north curr_x (curr_y + 1) board

(** Helper function to determine if black can play. *)
let rec can_black_play_helper = function
  | Black :: _ -> true
  | White :: tail -> can_black_play_helper tail
  | _ -> false

(** Determines if black's move validates the rules of othello in a specific
    direction provided by a list of pieces*)
let can_black_play = function
  | White :: tail -> can_black_play_helper tail
  | _ -> false

(** Helper function to determine if white can play. *)
let rec can_white_play_helper = function
  | White :: _ -> true
  | Black :: tail -> can_white_play_helper tail
  | _ -> false

(** Determines if white's move validates the rules of othello in a specific
    direction provided by a list of pieces*)
let can_white_play = function
  | Black :: tail -> can_white_play_helper tail
  | _ -> false

(** Checks if a move is valid for a given piece on the board based on the rules
    of othello and returns a bool true or false*)
let is_legit board curr_x curr_y piece =
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> Empty
  then false
  else
    let f n =
      (match piece with
      | Black -> can_black_play
      | White -> can_white_play
      | Empty -> failwith "color required")
        (List.map (fun (x, y) -> get_element x y board) n)
    in
    f (northeast (curr_x + 1) (curr_y + 1) board)
    || f (east (curr_x + 1) curr_y board)
    || f (southeast (curr_x + 1) (curr_y - 1) board)
    || f (south curr_x (curr_y - 1) board)
    || f (southwest (curr_x - 1) (curr_y - 1) board)
    || f (west (curr_x - 1) curr_y board)
    || f (northwest (curr_x - 1) (curr_y + 1) board)
    || f (north curr_x (curr_y + 1) board)

(** Finds all valid moves for a given piece on the board. *)
let find_all_valid_moves color board =
  let is_valid_move row col =
    match get_element col row board with
    | Empty -> is_legit board col row color
    | _ -> false
  in
  let rec find_moves row col acc =
    if row > 7 then acc
    else if col > 7 then find_moves (row + 1) 0 acc
    else if is_valid_move row col then
      find_moves row (col + 1) ((row, col) :: acc)
    else find_moves row (col + 1) acc
  in
  find_moves 0 0 []

(** Flips pieces in the northeast direction starting from a point. *)
let rec northeast_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    northeast_flip (curr_x + 1) (curr_y + 1) color out_board

(** Flips pieces in the east direction starting from a point. *)
let rec east_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    east_flip (curr_x + 1) curr_y color out_board

(** Flips pieces in the southeast direction starting from a point. *)
let rec southeast_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    southeast_flip (curr_x + 1) (curr_y - 1) color out_board

(** Flips pieces in the south direction starting from a point. *)
let rec south_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    south_flip curr_x (curr_y - 1) color out_board

(** Flips pieces in the southwest direction starting from a point. *)
let rec southwest_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    southwest_flip (curr_x - 1) (curr_y - 1) color out_board

(** Flips pieces in the west direction starting from a point. *)
let rec west_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    west_flip (curr_x - 1) curr_y color out_board

(** Flips pieces in the northwest direction starting from a point. *)
let rec northwest_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    northwest_flip (curr_x - 1) (curr_y + 1) color out_board

(** Flips pieces in the north direction starting from a point. *)
let rec north_flip curr_x curr_y color board =
  let color_opp =
    match color with
    | Black -> White
    | White -> Black
    | Empty -> failwith "color must be defined"
  in
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> color_opp
  then board
  else
    let out_board = update_element curr_x curr_y color board in
    north_flip curr_x (curr_y + 1) color out_board

(** Places a piece and flips adjacent pieces according to the rules of othello. *)
let place_and_flip_pieces curr_x curr_y piece board =
  if
    curr_x > 7 || curr_y > 7 || curr_x < 0 || curr_y < 0
    || get_element curr_x curr_y board <> Empty
  then board
  else
    let f n =
      (match piece with
      | Black -> can_black_play
      | White -> can_white_play
      | Empty -> failwith "color required")
        (List.map (fun (x, y) -> get_element x y board) n)
    in
    let board_with_piece = place_piece curr_x curr_y piece board in
    let board1 =
      if f (northeast (curr_x + 1) (curr_y + 1) board) then
        northeast_flip (curr_x + 1) (curr_y + 1) piece board_with_piece
      else board_with_piece
    in
    let board2 =
      if f (east (curr_x + 1) curr_y board) then
        east_flip (curr_x + 1) curr_y piece board1
      else board1
    in
    let board3 =
      if f (southeast (curr_x + 1) (curr_y - 1) board) then
        southeast_flip (curr_x + 1) (curr_y - 1) piece board2
      else board2
    in
    let board4 =
      if f (south curr_x (curr_y - 1) board) then
        south_flip curr_x (curr_y - 1) piece board3
      else board3
    in
    let board5 =
      if f (southwest (curr_x - 1) (curr_y - 1) board) then
        southwest_flip (curr_x - 1) (curr_y - 1) piece board4
      else board4
    in
    let board6 =
      if f (west (curr_x - 1) curr_y board) then
        west_flip (curr_x - 1) curr_y piece board5
      else board5
    in
    let board7 =
      if f (northwest (curr_x - 1) (curr_y + 1) board) then
        northwest_flip (curr_x - 1) (curr_y + 1) piece board6
      else board6
    in
    if f (north curr_x (curr_y + 1) board) then
      north_flip curr_x (curr_y + 1) piece board7
    else board7
