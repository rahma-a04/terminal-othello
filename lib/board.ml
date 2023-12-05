(** creates the board of Othello, and provide a function that prints it out in
    terminal. *)
open Piece
open Position

type piece =
  | Black
  | White
  | Empty

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

let rec to_list_small lst =
  match lst with
  | [] -> []
  | h :: t ->
      if h = Empty then "Empty" :: to_list_small t
      else if h = Black then "Black" :: to_list_small t
      else "White" :: to_list_small t

let rec to_list board =
  match board with
  | [] -> []
  | h :: t -> to_list_small h :: to_list t

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_string h;
      print_string " | ";
      print_list t

let board_row (lst : string list) : unit =
  print_string "| ";
  print_list lst

let rec print_list_of_lists = function
  | [] -> ()
  | h :: t ->
      board_row h;
      print_newline ();
      print_string "------------------------------------------------------------------";
      print_newline ();
      print_list_of_lists t

let print_board_top (lst : string list list) : unit =
  print_string "------------------------------------------------------------------";
  print_newline ();
  print_list_of_lists lst

let print_board (board : piece list list) : unit =
  print_board_top (to_list board)

let rec count_number_of_objs_in_list lst (obj : piece) =
  match lst with
  | [] -> 0
  | h :: t ->
      if h = obj then 1 + count_number_of_objs_in_list t obj
      else count_number_of_objs_in_list t obj

let rec count_pieces (board : piece list list) (color : piece) =
  match board with
  | [] -> 0
  | h :: t -> count_number_of_objs_in_list h color + count_pieces t color

let is_board_filled (board : piece list list) =
  if count_pieces board Empty > 0 then false else true

let get_element row col board =
  try
    let selected_row = List.nth board row in
    List.nth selected_row col
  with Failure _ -> failwith "Invalid row or column index"

let valid_move row col board =
  if row > 7 || col > 7 then false
  else if get_element row col board = Empty then true
  else false

let rec update_element row col value board =
  List.mapi
    (fun i r ->
      if i = row then List.mapi (fun j v -> if j = col then value else v) r
      else r)
    board

let place_piece x y (color : piece) board = update_element x y color board

module LegitMove = struct
  let rec northeast curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: northeast (curr_x + 1) (curr_y + 1) board

  let rec east curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: east (curr_x + 1) curr_y board

  let rec southeast curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: southeast (curr_x + 1) (curr_y - 1) board

  let rec south curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: south curr_x (curr_y - 1) board

  let rec southwest curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: southwest (curr_x - 1) (curr_y - 1) board

  let rec west curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: west (curr_x - 1) curr_y board

  let rec northwwest curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: northwwest (curr_x - 1) (curr_y + 1) board

  let rec north curr_x curr_y board =
    if curr_x < 0 || curr_y < 0 || curr_x > 7 || curr_y > 7 then []
    else
      match get_element curr_x curr_y board with
      | Empty -> []
      | _ -> (curr_x, curr_y) :: north curr_x (curr_y + 1) board

  let rec can_black_play_helper = function
    | [] -> false
    | Black :: _ -> true
    | White :: tail -> can_black_play_helper tail

  let can_black_play board =
    can_black_play_helper (List.map (fun (x, y) -> get_element x y board) (northeast 1 1 board))

  let rec can_white_play_helper = function
    | [] -> false
    | White :: _ -> true
    | Black :: tail -> can_white_play_helper tail

  let can_white_play board =
    can_white_play_helper (List.map (fun (x, y) -> get_element x y board) (northeast 1 1 board))

  let is_legit board curr_x curr_y piece =
    let f n =
      (match piece with Black -> can_black_play | White -> can_white_play) n
    in
    f (northeast (curr_x + 1) (curr_y + 1) board)
    || f (east (curr_x + 1) curr_y board)
    || f (southeast (curr_x + 1) (curr_y - 1) board)
    || f (south curr_x (curr_y - 1) board)
    || f (southwest (curr_x - 1) (curr_y - 1) board)
    || f (west (curr_x - 1) curr_y board)
    || f (northwwest (curr_x - 1) (curr_y + 1) board)
    || f (north curr_x (curr_y + 1) board)
end

let valid_move row col color board =
  LegitMove.is_legit board row col color