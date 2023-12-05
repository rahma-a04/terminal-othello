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
