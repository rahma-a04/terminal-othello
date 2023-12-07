type piece =
  | Black
  | White
  | Empty

val empty_board : piece list list
(** The empty Othello board. *)

val to_list_small : piece list -> string list
(** Converts a piece list to a string list. *)

val print_board : piece list list -> unit
(** Prints the current board in utop, takes in a board. *)

val count_number_of_objs_in_list : piece list -> piece -> int
(** Counts the number of specific pieces in a list. *)

val count_pieces : piece list list -> piece -> int
(** Count the number of specific colored pieces in board, takes in a board and a
    color and returns an integer. *)

val is_board_filled : piece list list -> bool
(** Checks whether all spaces in a board is taken up by a piece. Takes in a
    board and returns a boolean. *)

val to_list : piece list list -> string list list
(** Converts a board of pieces to a board of numbers with Empty corresponding
    with "Empty", Black corresponding with "Black", and White corresponding with
    "White". *)

val get_element : int -> int -> piece list list -> piece
(** Retrieves the element at a specified row and column from the board. *)

(*val valid_move : int -> int -> piece list list -> bool (** Checks whether the
  location has a piece on it already and whether it is out of scope of the
  board. *) *)

val place_piece : int -> int -> piece -> piece list list -> piece list list
(** Places the piece in the desired location (x, y). *)

val is_legit : piece list list -> int -> int -> piece -> bool
(** Checks to see if move is legal based on rules of othello*)

val find_all_valid_moves : piece -> piece list list -> (int * int) list
(** Finds all valid moves for a given piece on the board. *)

val place_and_flip_pieces :
  int -> int -> piece -> piece list list -> piece list list
(** Places a piece on the board, flips the opponent's pieces according to game
    rules, and returns the updated board. *)
