(** The computer player of the game with four possible levels: easy, medium,
    hard, and god mode. *)

val generateMoveHelper : (int * int) list -> int -> int -> int * int
(** Randomly selects possible move from all possible moves *)

val generateMoveEasy :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Uses generateMoveHelper to select a random move out of all possible moves *)

val generateMoveMedium :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the move that captures the most number of pieces out of out of all
    possible moves *)

val evaluate_move : int * int -> Board.piece -> int
(** Evaluates the weight of a specific move (x, y) based on a predefined weight
    matrix. This function returns the weight score of the move for a given
    color. *)

val generateMoveHard :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the best move based on preset weight matrix, and if a tie occurs,
    the algorithm takes preference to the move that captures the most number of
    pieces *)

val evaluate_move_extreme : int * int -> Board.piece -> Board.board -> int * int
(** Evaluates the move (x, y) for the extreme AI level, combining the static
    weight matrix evaluation with dynamic game factors. It considers both the
    move's strategic importance and the number of opponent pieces that would be
    flipped. Additional consideration is given for controlling edges and
    corners. Returns a tuple of the move's weight and the count of flipped
    pieces. *)

val alpha_beta_prune :
  Board.board -> int -> int ref -> int ref -> Board.piece -> bool -> int * int
(** Implements the Minimax algorithm with Alpha-Beta pruning to optimize the
    AI's move selection. It evaluates moves recursively up to a given depth,
    pruning branches that don't need to be explored. The function takes the
    current board, search depth, alpha, beta, current player's color, and a
    boolean indicating if the current player is maximizing or minimizing. It
    returns the best move found at that depth. *)

val generateMoveExtreme :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Generates a move for the extreme AI level. This function applies the
    alpha-beta pruning algorithm & evaluate move extreme to evaluate moves
    several steps ahead, *)
