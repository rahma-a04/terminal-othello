val generateMoveEasy :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects a random move out of all possible moves *)

val generateMoveMedium :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the move that captures the most number of pieces out of out of all
    possible moves *)

val generateMoveHard :
  (int * int) list -> Board.board -> Board.piece -> int * int
(** Selects the best move based on preset weight matrix, and if a tie occurs,
    the algorithm takes preference to the move that captures the most number of
    pieces *)

val generateMoveExtreme :
  (int * int) list -> Board.board -> Board.piece -> int * int

val generateMoveHelper : (int * int) list -> int -> int -> int * int
