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
    ( "single row (multiple different colors)" >:: fun _ ->
      assert_equal
        [
          [
            "Black";
            "White";
            "Empty";
            "Black";
            "Black";
            "Black";
            "Black";
            "Black";
          ];
        ]
        (Board.to_list
           [ [ Black; White; Empty; Black; Black; Black; Black; Black ] ]) );
    ( "two rows (Empty)" >:: fun _ ->
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
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "two rows (Black)" >:: fun _ ->
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
           [
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
           ]) );
    ( "two rows (White)" >:: fun _ ->
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
           [
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
           ]) );
    ( "starting position" >:: fun _ ->
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
          [
            "Empty";
            "Empty";
            "Empty";
            "Black";
            "White";
            "Empty";
            "Empty";
            "Empty";
          ];
          [
            "Empty";
            "Empty";
            "Empty";
            "White";
            "Black";
            "Empty";
            "Empty";
            "Empty";
          ];
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
        (Board.to_list Board.empty_board) );
    (* count_number_of_objs_in_list tests *)
    ( "Empty list" >:: fun _ ->
      assert_equal 0 (Board.count_number_of_objs_in_list [] Empty) );
    ( "Empty piece list" >:: fun _ ->
      assert_equal 8
        (Board.count_number_of_objs_in_list
           [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
           Empty) );
    ( "Black piece list" >:: fun _ ->
      assert_equal 8
        (Board.count_number_of_objs_in_list
           [ Black; Black; Black; Black; Black; Black; Black; Black ]
           Black) );
    ( "White piece list" >:: fun _ ->
      assert_equal 8
        (Board.count_number_of_objs_in_list
           [ White; White; White; White; White; White; White; White ]
           White) );
    ( "Empty piece list" >:: fun _ ->
      assert_equal 0
        (Board.count_number_of_objs_in_list
           [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
           Black) );
    ( "Varied piece list" >:: fun _ ->
      assert_equal 0
        (Board.count_number_of_objs_in_list
           [ Empty; Empty; White; Empty; White; Empty; Empty; Empty ]
           Black) );
    ( "Another varied piece list" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ Empty; Black; Empty; Empty; Empty; White; Empty; Empty ]
           Black) );
    ( "Edge case: head for Black" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ Black; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
           Black) );
    ( "Edge case : tail for Black" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Black ]
           Black) );
    ( "Edge case: head for White" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ White; Empty; Empty; Empty; Empty; Empty; Empty; Empty ]
           White) );
    ( "Edge case : tail for White" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; White ]
           White) );
    ( "Edge case: head for Empty" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ Empty; White; White; White; White; White; White; White ]
           Empty) );
    ( "Edge case : tail for Empty" >:: fun _ ->
      assert_equal 1
        (Board.count_number_of_objs_in_list
           [ White; White; White; White; White; White; White; Empty ]
           Empty) );
    (* Count pieces tests *)
  ]

let suite = "test suite for Othello" >::: List.flatten [ board_tests ]
let () = run_test_tt_main suite
