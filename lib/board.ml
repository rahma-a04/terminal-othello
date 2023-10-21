(** creates the board of othello, and provide a function that prints it out in
    terminal. *)

let empty_board =
  [
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0; 0 ];
  ]

let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " | ";
      print_list t

let board_row (lst : int list) : unit =
  print_string "| ";
  print_list lst

let rec print_list_of_lists = function
  | [] -> ()
  | h :: t ->
      board_row h;
      print_newline ();
      print_string "---------------------------------";
      print_newline ();
      print_list_of_lists t

let print_board_top (lst : int list list) : unit =
  print_string "---------------------------------";
  print_newline ();
  print_list_of_lists lst

let print_board (board : int list list) : unit = print_board_top board

let rec count_number_of_objs_in_list lst (obj : int) =
  match lst with
  | [] -> 0
  | h :: t ->
      if h = obj then 1 + count_number_of_objs_in_list t obj
      else count_number_of_objs_in_list t obj

let rec count_pieces (board : int list list) (color : int) =
  match board with
  | [] -> 0
  | h :: t -> count_number_of_objs_in_list h color + count_pieces t color

let is_board_filled (board : int list list) =
  if count_pieces board 1 + count_pieces board 2 < 64 then false else true
