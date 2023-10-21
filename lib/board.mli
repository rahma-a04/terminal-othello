val print_board : int list list -> unit
(** Prints the current board in utop, takes in a board. *)

val count_pieces : int list list -> int -> int
(** Count the number of specific colored pieces in board, takes in a board and a
    color and returns an integer. *)

val is_board_filled : int list list -> bool
(** Checks whether all spaces in a board is taken up by a piece. Takes in a
    board and returns a boolean. *)
