open Board

type game_state = {
  board : board;
  move_num : int;
  player : piece;
}

type game = game_state list

let new_game = failwith "unimplemented"
let update = failwith "unimplemneted"
let print_current_state = failwith "unimplemented"
let print_previous_games = failwith "unimplemented"
