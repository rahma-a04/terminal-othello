open Board

exception End_game

type game_state
(** Type representing a single snapshot of the game at any given point in time.
    Includes the current board, the number of completed moves, and the current
    player *)

type game
(** Type representing an entire game, including all states in the "history" of
    the game.*)

val new_game : game
(** Initializes a new game of Othello. *)

val board_of_game : game -> board
val player_of_game : game -> piece

val update : int -> int -> game -> game
(** [update player row col game] is the game after [player] has placed a new
    piece in the given [row] and [col]. Requires: [(row, col)] is a valid
    location to place a piece. *)

val print_current_game : game -> unit
(** [print_current_game game] prints the latest state of the game, including the
    player whose turn it is, the number of completed moves, and the current
    board. *)

val print_previous_games : game -> unit
(** [print_previous_games] prints the entire history of the current game,
    including all previous states of the board. *)
