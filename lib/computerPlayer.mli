val generateMoveEasy :
  (int * int) list -> Board.board -> Board.piece -> int * int

val generateMoveMedium :
  (int * int) list -> Board.board -> Board.piece -> int * int

val generateMoveHard :
  (int * int) list -> Board.board -> Board.piece -> int * int

val generateMoveHelper : (int * int) list -> int -> int -> int * int
