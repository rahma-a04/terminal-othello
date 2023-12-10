(** The computer player of the game with four possible levels: easy, medium,
    hard, and god mode (extreme). *)

val generate_move_easy :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects a random move out of all possible moves *)

val generate_move_medium :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the move that captures the most number of pieces out of out of all
    possible moves *)

val generate_move_hard :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the best move based on preset weight matrix, and if a tie occurs,
    the algorithm takes preference to the move that captures the most number of
    pieces *)

val generate_move_extreme :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Generates a move for the extreme AI level. This function applies the
    alpha-beta pruning algorithm & evaluate move extreme to evaluate moves
    several steps ahead. It evaluates moves recursively up to a given depth,
    pruning branches that don't need to be explored. Evaluates the move (x, y)
    for the extreme AI level, combining the static weight matrix evaluation with
    dynamic game factors. It considers both the move's strategic importance and
    the number of opponent pieces that would be flipped. Additional
    consideration is given for controlling edges and corners. It returns the
    best move found at that depth.*)
