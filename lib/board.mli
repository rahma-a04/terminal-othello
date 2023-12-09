type piece =
  | Black
  | White
  | Empty

type board = piece list list

val empty_board : board
(** The empty Othello board. *)

val to_list_small : piece list -> string list
(** Converts a piece list to a string list. *)

val print_board : board -> unit
(** Prints the current board in utop, takes in a board. *)

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

(* val to_list_small_unicode : piece list -> string list val to_list_unicode :
   board -> string list list *)

val to_piece_list : board -> piece list list
(** Converts a board into a list of Piece lists *)

val get_element : int -> int -> board -> piece
(** Retrieves the element at a specified row and column from the board. *)

(*val valid_move : int -> int -> piece list list -> bool (** Checks whether the
  location has a piece on it already and whether it is out of scope of the
  board. *) *)

val place_piece : int -> int -> piece -> board -> board
(** Places the piece in the desired location (x, y). *)

val is_legit : board -> int -> int -> piece -> bool
(** Checks to see if move is legal based on rules of othello*)

val find_all_valid_moves : piece -> board -> (int * int) list
(** Finds all valid moves for a given piece on the board. *)

val place_and_flip_pieces : int -> int -> piece -> board -> board
(** Places a piece on the board, flips the opponent's pieces according to game
    rules, and returns the updated board. *)

(* open Map open Position (* module type Positions_mapType = sig type t = int *
   int

   val find_opt : t -> t -> int end *)

   module type LegitMoveType = sig type piece = | Black | White

   val northeast : int -> int -> (int * int) list val east : int -> int -> (int
   * int) list val southeast : int -> int -> (int * int) list val south : int ->
   int -> (int * int) list val southwest : int -> int -> (int * int) list val
   west : int -> int -> (int * int) list val northwwest : int -> int -> (int *
   int) list val north : int -> int -> (int * int) list val
   can_black_play_helper : piece option list -> bool val can_black_play : piece
   option list -> bool val can_white_play_helper : piece option list -> bool val
   can_white_play : piece option list -> bool val is_legit : piece
   Positions_map.t -> int -> int -> piece -> bool (* val is_legit :
   Positions_map_sig.key Map.t -> int -> int -> piece -> bool *) end

   (*module Positions_map : Positions_mapType module LegitMove :
   LegitMoveType*) *)
