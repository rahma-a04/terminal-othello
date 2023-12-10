(** Creates the board of Othello, and provide a function that prints it out in
    terminal. *)

(** A piece is the possible pieces that can occupy any spot on the 8 x 8 othello
    board. *)
type piece =
  | Black
  | White
  | Empty  (** The type of a single piece on the Othello board.*)

type board = piece list list
(** The type of a board of Othello. Represents all pieces placed on their
    respective positions. *)

val black_circle_code : string
(** Unicode value for a black circle character. *)

val white_circle_code : string
(** Unicode value for a white circle character. *)

val empty_code : string
(** String representation of our empty pieces. *)

val empty_board : board
(** The starting position of an Othello board. *)

val to_list_small : piece list -> string list
(** Converts a piece list to a string list. *)

val print_board : board -> unit
(** Prints the current board in utop with letters on top and numbers on the
    side. Takes in a board. *)

val count_number_of_objs_in_list : piece list -> piece -> int
(** Counts the number of specific pieces in a list. *)

val count_pieces : board -> piece -> int
(** Count the number of specific colored pieces in board, takes in a board and a
    color and returns an integer. *)

val is_board_filled : board -> bool
(** Checks whether all spaces in a board is taken up by a piece. Takes in a
    board and returns a boolean. *)

val to_list : board -> string list list
(** Converts a board of pieces to a board of numbers with Empty corresponding
    with "Empty", Black corresponding with "Black", and White corresponding with
    "White". *)

val to_piece_list : board -> piece list list
(** Converts a board into a list of Piece lists *)

val get_element : int -> int -> board -> piece
(** Retrieves the element at a specified row and column from the board. *)

val place_piece : int -> int -> piece -> board -> board
(** Places the piece in the desired location (x, y). *)

val is_legit : board -> int -> int -> piece -> bool
(** Checks to see if move is legal based on rules of othello. *)

val find_all_valid_moves : piece -> board -> (int * int) list
(** Finds all valid moves for a given piece on the board. *)

val place_and_flip_pieces : int -> int -> piece -> board -> board
(** Places a piece on the board, flips the opponent's pieces according to game
    rules, and returns the updated board. *)
