type game_state
type game

val new_game : game
val update : game -> int -> int -> game
val print_current_state : game -> unit
val print_previous_games : game -> unit
