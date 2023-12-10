(** The User Interface module that provides functionality for certain user
    inputs. *)

open Board

exception End_game
(** Exception for when a game is over. *)

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
(** Returns the [Board.board] value associated with [game] *)

val player_of_game : game -> piece
(** Returns the [Board.piece] value of the current player of [game] *)

val skip_turn : game -> game
(** Skips the turn of the player currently playing [game]. *)

val update : int -> int -> game -> game
(** [update player row col game] is the game after [player] has placed a new
    piece in the given [row] and [col]. Requires: [(row, col)] is a valid
    location to place a piece. *)

val revert : game -> int -> game
(** [revert game n] reverts [game] to the nth move. Raises: Failure if n is
    outside the game's move range *)

val print_current_game : game -> unit
(** [print_current_game game] prints the latest state of the game, including the
    player whose turn it is, the number of completed moves, and the current
    board. *)

val print_previous_games : game -> unit
(** [print_previous_games] prints the entire history of the current game,
    including all previous states of the board. *)
