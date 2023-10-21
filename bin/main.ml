let rec repl (eval : string -> string) : unit =
  print_string "> ";
  let input = read_line () in
  match input with
  | "" -> print_endline "bye"
  | _ -> input |> eval |> print_endline;
         repl eval

let () = 
  print_endline "Welcome to Othello! Start game? (Y/N)";
  (* print_board note: when everyone pushes *)
  print_string "> ";
  let response = read_line () in
  match response with
  | "Y" -> print_endline("buffer: start") (*note: change it so that it's case-insensitive*)
  | "N" -> print_endline("buffer: end")
  | _ -> print_endline("Error: invalid response")