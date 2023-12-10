(* Testing Plan: For our testing plan, we combined automatic tests with ounit2
   and manual tests with utop and the terminal to ensure good coverage with all
   of our functions.

   For our automatic tests, we tested each function in Board.mli, as those were
   the main functions that our user interface calls to run the game properly. We
   attempted to get full coverage with glass box testing. By looking at each
   line of the implementation, we were able to write corresponding test cases.
   We also attempted to test all the edge cases, mainly the top left and bottom
   right corners of the board. In testing all possible cases, we ensure that all
   functions in Board.mli is functioning exactly as expected.

   For our manual tests, we tested our computer players and the basic game
   functionality in terminal and utop. We used black box testing, as we were
   just observing the behaviors of the computer players under all possible
   inputs. This ensures that a typical person would be able to play the game
   without bugs. We played over 100 games to fully test our functionality in
   bin/main and ui.mli.

   In total, we used a combination of glass-box and black-box testing to ensure
   that our game functions as intended under all possible scenarios. *)

open OUnit2
open Othello
open Board

let black_circle_code = "\u{25CB}"
let white_circle_code = "\u{25CF}"
let empty_code = " "

let board_tests =
  [
    (* To_List_Small tests *)
    ("empty piece list" >:: fun _ -> assert_equal [] (Board.to_list_small []));
    ( "piece list with one black piece" >:: fun _ ->
      assert_equal [ black_circle_code ] (Board.to_list_small [ Black ]) );
    ( "piece list with one white piece" >:: fun _ ->
      assert_equal [ white_circle_code ] (Board.to_list_small [ White ]) );
    ( "piece list with one empty piece" >:: fun _ ->
      assert_equal [ empty_code ] (Board.to_list_small [ Empty ]) );
    ( "piece list with more than one black piece" >:: fun _ ->
      assert_equal
        [ black_circle_code; black_circle_code ]
        (Board.to_list_small [ Black; Black ]) );
    ( "piece list with one white piece" >:: fun _ ->
      assert_equal
        [ white_circle_code; white_circle_code ]
        (Board.to_list_small [ White; White ]) );
    ( "piece list with one empty piece" >:: fun _ ->
      assert_equal [ empty_code; empty_code ]
        (Board.to_list_small [ Empty; Empty ]) );
    ( "piece list with two different pieces" >:: fun _ ->
      assert_equal
        [ black_circle_code; white_circle_code ]
        (Board.to_list_small [ Black; White ]) );
    ( "piece list with two different pieces part two" >:: fun _ ->
      assert_equal
        [ empty_code; black_circle_code ]
        (Board.to_list_small [ Empty; Black ]) );
    ( "piece list with three different pieces" >:: fun _ ->
      assert_equal
        [ empty_code; black_circle_code; white_circle_code ]
        (Board.to_list_small [ Empty; Black; White ]) );
    ( "piece list with three different pieces (different order)" >:: fun _ ->
      assert_equal
        [ empty_code; white_circle_code; black_circle_code ]
        (Board.to_list_small [ Empty; White; Black ]) );
    (* To_List tests *)
    ("Empty board" >:: fun _ -> assert_equal [] (Board.to_list []));
    ( "Empty board (list of lists)" >:: fun _ ->
      assert_equal [ [] ] (Board.to_list [ [] ]) );
    ( "single row" >:: fun _ ->
      assert_equal
        [
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
        ]
        (Board.to_list
           [ [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ] ]) );
    ( "single row (different color)" >:: fun _ ->
      assert_equal
        [
          [
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
          ];
        ]
        (Board.to_list
           [ [ White; White; White; White; White; White; White; White ] ]) );
    ( "single row (different color again)" >:: fun _ ->
      assert_equal
        [
          [
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
          ];
        ]
        (Board.to_list
           [ [ Black; Black; Black; Black; Black; Black; Black; Black ] ]) );
    ( "single row (multiple different colors)" >:: fun _ ->
      assert_equal
        [
          [
            black_circle_code;
            white_circle_code;
            empty_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
            black_circle_code;
          ];
        ]
        (Board.to_list
           [ [ Black; White; Empty; Black; Black; Black; Black; Black ] ]) );
    ( "two rows (Empty)" >:: fun _ ->
      assert_equal
        [
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
        ]
        (Board.to_list
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "two rows (White)" >:: fun _ ->
      assert_equal
        [
          [
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
          ];
          [
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
            white_circle_code;
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
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
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
    (* Count pieces tests *)
    ("Empty list" >:: fun _ -> assert_equal 0 (Board.count_pieces [] Empty));
    ( "Empty board" >:: fun _ ->
      assert_equal 64
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           Empty) );
    ( "Empty board, different piece" >:: fun _ ->
      assert_equal 0
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           Black) );
    ( "Empty board, different piece, again" >:: fun _ ->
      assert_equal 0
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           White) );
    ( "full starting position: empty" >:: fun _ ->
      assert_equal 60
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           Empty) );
    ( "full starting position: black" >:: fun _ ->
      assert_equal 2
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           Black) );
    ( "full starting position: white" >:: fun _ ->
      assert_equal 2
        (Board.count_pieces
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]
           White) );
    ( "Edge case: Black" >:: fun _ ->
      assert_equal 4
        (Board.count_pieces
           [
             [ Black; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Black ];
           ]
           Black) );
    ( "Edge case: white" >:: fun _ ->
      assert_equal 4
        (Board.count_pieces
           [
             [ White; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; White ];
           ]
           White) );
    (* is_board_filled tests *)
    ("empty board" >:: fun _ -> assert_bool "failed" (Board.is_board_filled []));
    ( "empty piece board" >:: fun _ ->
      assert_bool "failed" (Bool.not (Board.is_board_filled [ [ Empty ] ])) );
    ( "one row of empty piece board" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [ [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ] ]))
    );
    ( "full empty board" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
              ])) );
    ( "Starting pos" >:: fun _ ->
      assert_bool "failed" (Bool.not (Board.is_board_filled Board.empty_board))
    );
    ( "edge case: white" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [
                [ White; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; White ];
              ])) );
    ( "edge case: black" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [
                [ Black; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
                [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Black ];
              ])) );
    ( "only one empty piece board" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [
                [ Empty; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
              ])) );
    ( "only one empty piece board - other end" >:: fun _ ->
      assert_bool "failed"
        (Bool.not
           (Board.is_board_filled
              [
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Black ];
                [ Black; Black; Black; Black; Black; Black; Black; Empty ];
              ])) );
    ( "board is filled (black)" >:: fun _ ->
      assert_bool "failed"
        (Board.is_board_filled
           [
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
             [ Black; Black; Black; Black; Black; Black; Black; Black ];
           ]) );
    ( "board is filled (white)" >:: fun _ ->
      assert_bool "failed"
        (Board.is_board_filled
           [
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
             [ White; White; White; White; White; White; White; White ];
           ]) );
    (* Get_element tests *)
    ( "(0, 0) : empty" >:: fun _ ->
      assert_equal Board.Empty
        (Board.get_element 0 0
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "(0, 0) : Black" >:: fun _ ->
      assert_equal Board.Black
        (Board.get_element 0 0
           [
             [ Black; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "(0, 0) : White" >:: fun _ ->
      assert_equal Board.White
        (Board.get_element 0 0
           [
             [ White; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "(7, 7) : empty" >:: fun _ ->
      assert_equal Board.Empty
        (Board.get_element 0 0
           [
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
           ]) );
    ( "(7, 7) : Black" >:: fun _ ->
      assert_equal Board.Black
        (Board.get_element 0 0
           [
             [ Black; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Black ];
           ]) );
    ( "(7, 7) : White" >:: fun _ ->
      assert_equal Board.White
        (Board.get_element 0 0
           [
             [ White; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Black; White; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; White; Black; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; Empty ];
             [ Empty; Empty; Empty; Empty; Empty; Empty; Empty; White ];
           ]) );
    (* Place_piece test *)
    ( "starting position" >:: fun _ ->
      assert_equal
        [
          [
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
        ]
        (Board.to_list (Board.place_piece 0 0 White Board.empty_board)) );
    ( "other end" >:: fun _ ->
      assert_equal
        [
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
          ];
        ]
        (Board.to_list (Board.place_piece 7 7 White Board.empty_board)) );
    ( "starting position" >:: fun _ ->
      assert_equal
        [
          [
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
        ]
        (Board.to_list (Board.place_piece 0 0 Black Board.empty_board)) );
    ( "other end" >:: fun _ ->
      assert_equal
        [
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
            white_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            white_circle_code;
            black_circle_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
          ];
          [
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            empty_code;
            black_circle_code;
          ];
        ]
        (Board.to_list (Board.place_piece 7 7 Black Board.empty_board)) );
    ( "is_legit false test" >:: fun _ ->
      let board = Board.empty_board in
      assert_equal false (Board.is_legit board 2 3 Black) );
    ( "is_legit true test" >:: fun _ ->
      let board = Board.empty_board in
      assert_equal true (Board.is_legit board 4 2 Black) );
    ( "find_all_valid_moves test" >:: fun _ ->
      let board = Board.empty_board in
      let expected_moves = [ (5, 3); (4, 2); (3, 5); (2, 4) ] in
      assert_equal expected_moves (Board.find_all_valid_moves Black board) );
    ( "place_and_flip_pieces true test" >:: fun _ ->
      let board = Board.empty_board in
      let board = Board.place_and_flip_pieces 4 2 Black board in
      assert_equal Board.Black (Board.get_element 4 3 board) );
  ]

let suite = "test suite for Othello" >::: List.flatten [ board_tests ]
let () = run_test_tt_main suite
