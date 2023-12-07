let rec generateMoveHelper (possibleMoves : (int * int) list) (v : int)
    (currentNum : int) : int * int =
  match possibleMoves with
  | (x, y) :: tail ->
      if currentNum <> v then generateMoveHelper tail v (currentNum + 1)
      else (x, y)
  | [] -> raise (Invalid_argument "no more moves :(")

let generateMove (possibleMoves : (int * int) list) : int * int =
  let movePos = Random.int (List.length possibleMoves) in
  generateMoveHelper possibleMoves movePos 0
