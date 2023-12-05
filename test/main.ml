open OUnit2
open Othello

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  Printf.sprintf "[%s]" (pp_elts lst)

let pp_string s = "\"" ^ s ^ "\""

let board_tests =
  [
    (* To_List_Small tests *)
    ("empty piece list" >:: fun _ -> assert_equal [] (Board.to_list_small []));
    ( "piece list with one black piece" >:: fun _ ->
      assert_equal [ "Black" ] (Board.to_list_small [ Black ]) );
    ( "piece list with one white piece" >:: fun _ ->
      assert_equal [ "White" ] (Board.to_list_small [ White ]) );
    ( "piece list with one empty piece" >:: fun _ ->
      assert_equal [ "Empty" ] (Board.to_list_small [ Empty ]) );
    ( "piece list with more than one black piece" >:: fun _ ->
      assert_equal [ "Black"; "Black" ] (Board.to_list_small [ Black; Black ])
    );
    ( "piece list with one white piece" >:: fun _ ->
      assert_equal [ "White"; "White" ] (Board.to_list_small [ White; White ])
    );
    ( "piece list with one empty piece" >:: fun _ ->
      assert_equal [ "Empty"; "Empty" ] (Board.to_list_small [ Empty; Empty ])
    );
    ( "piece list with two different pieces" >:: fun _ ->
      assert_equal [ "Black"; "White" ] (Board.to_list_small [ Black; White ])
    );
    ( "piece list with two different pieces part two" >:: fun _ ->
      assert_equal [ "Empty"; "Black" ] (Board.to_list_small [ Empty; Black ])
    );
    ( "piece list with three different pieces" >:: fun _ ->
      assert_equal
        [ "Empty"; "Black"; "White" ]
        (Board.to_list_small [ Empty; Black; White ]) );
    ( "piece list with three different pieces (different order)" >:: fun _ ->
      assert_equal
        [ "Empty"; "White"; "Black" ]
        (Board.to_list_small [ Empty; White; Black ]) );
    (* To_List tests *)
    ("Empty board" >:: fun _ -> assert_equal [] (Board.to_list []));
    ( "Empty board (list of lists)" >:: fun _ ->
      assert_equal [ [] ] (Board.to_list [ [] ]) );
    ( "single row" >:: fun _ ->
      assert_equal
        [
          [
            "Empty";
            "Empty";
            "Empty";
            "Empty";
            "Empty";
            "Empty";
            "Empty";
            "Empty";
          ];
        ]
        (Board.to_list
           [ [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ] ]) );
    ( "single row (different color)" >:: fun _ ->
      assert_equal
        [
          [
            "White";
            "White";
            "White";
            "White";
            "White";
            "White";
            "White";
            "White";
          ];
        ]
        (Board.to_list
           [ [ White; White; White; White; White; White; White; White ] ]) );
    ( "single row (different color again)" >:: fun _ ->
      assert_equal
        [
          [
            "Black";
            "Black";
            "Black";
            "Black";
            "Black";
            "Black";
            "Black";
            "Black";
          ];
        ]
        (Board.to_list
           [ [ Black; Black; Black; Black; Black; Black; Black; Black ] ]) );
  ]

let suite = "test suite for Othello" >::: List.flatten [ board_tests ]
let () = run_test_tt_main suite
