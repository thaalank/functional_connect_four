open OUnit2
open State

(********************************************************************
   Test Plan

   The most crucial aspect of a game is whether the game can reach
   completion, resulting in either a win/lose or a tie. Therefore, 
   we decided to use OUnit to test the State module, which takes care
   of all the necessary components of Connect 4 game play. We tested 
   the implementation of winning conditions such as vertical 4, 
   horizontal 4, and diagonal 4. We created these states using the drop
   function and make_start_state function in the State module. Thus, we're 
   testing those functions as well. We also have seperate tests for the drop
   function and make_start_state function, just to make sure it works properly. 

   We created various states of the board in order to cover all cases
   during game play. We initially used black-box testing to write up 
   test cases that must pass according to the Connect 4 rules. After 
   implementation, we used glass-box testing to test any edge cases 
   such as dropping tokens into invalid columns. Since our implementation of 
   the State module passes all of our tests, we know it's working correctly. 

   We chose to test the minimax algorithm in the minMax module manually through 
   playing against it. We know the algoirthm is working correctly because of the
   way it plays. The algorithm blocks the user from creating rows and columns
   with consecutive tokens and tries to prevent the user from winning. Further,
   the alg. tries to maximize it's own rows and columns with consecutive tokens
   and tries to win. Also, the alg. works differently from depth 3 to depth
   5, which is medium and hard in our game. The hard is significantly more
   difficult so we know it works. 

   We tested our GUI manually as well. Since it's a GUI we can visually see if 
   we are gettting what we want based on the inputs, setting, etc. 
   The only function we tested related to the GUI is [within], which is just
   as simple test to see if a point is inside a rectangle.
   The rest of the GUI related functions and modules we tested manually
   through play testing, as the actual internal representation of the aspects
   of the GUI matters less than how the game looks and feels when playing it.
 ********************************************************************)

(********************************************************************
   Here are some definitions for testing State. 
 ********************************************************************)

(* The starting state.
   5 E E E E E E E
   4 E E E E E E E
   3 E E E E E E E
   2 E E E E E E E
   1 E E E E E E E
   0 E E E E E E E
     0 1 2 3 4 5 6
*)
let start_state = make_start_state 6 7

(* The state following start_state after dropping a yellow token at column 0.
   5 E E E E E E E
   4 E E E E E E E
   3 E E E E E E E
   2 E E E E E E E
   1 E E E E E E E
   0 Y E E E E E E
     0 1 2 3 4 5 6
*)
let state_1 = drop Yellow 0 start_state

(* The state following state_1 after dropping a Red token at column 0.
   5 E E E E E E E
   4 E E E E E E E
   3 E E E E E E E
   2 E E E E E E E
   1 R E E E E E E
   0 Y E E E E E E
     0 1 2 3 4 5 6
*)
let state_2 = drop Red 0 state_1


(* The state following state_2 after dropping a few more tokens.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E E E E E
   2 R E E E E E E
   1 R E R E E E E
   0 Y E Y Y E E R
     0 1 2 3 4 5 6
*)
let state_3 = state_2 |> drop Yellow 2 |> drop Red 2 |> drop Yellow 3
              |> drop Red 0 |> drop Yellow 0 |> drop Red 0 |> drop Yellow 0
              |> drop Red 6

(* The state following state_3 that makes red win with a vertical 4.
   5 Y E E E E E E
   4 R E R E E E E
   3 Y E R E E E E
   2 R E R E E E E
   1 R E R E E E E
   0 Y E Y Y E E R
     0 1 2 3 4 5 6
*)
let state_4 = state_3 |> drop Red 2 |> drop Red 2 |> drop Red 2

(* The state following state_3 that makes yellow win with a vertical 4.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E Y E E E
   2 R E E Y E E E
   1 R E R Y E E E
   0 Y E Y Y E E R
     0 1 2 3 4 5 6
*)
let state_4' = state_3 |> drop Yellow 3 |> drop Yellow 3 |> drop Yellow 3

(* The state following state_3 that makes yellow win with a horizontal 4.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E E E E E
   2 R E E E E E E
   1 R E R E E E E
   0 Y Y Y Y E E R
     0 1 2 3 4 5 6
*)
let state_5 = state_3 |> drop Yellow 1

(* The state following state_3 that makes red win with a horizontal 4.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E E E E E
   2 R E E E E E E
   1 R R R R E E E
   0 Y R Y Y E E R
     0 1 2 3 4 5 6
*)
let state_5' = state_3 |> drop Red 1 |> drop Red 1 |> drop Red 3

(* The state following state_3 that makes yellow win with a diagonal 4.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E Y E E E
   2 R E Y R E E E
   1 R Y R R E E E
   0 Y R Y Y E E R
     0 1 2 3 4 5 6
*)
let state_6 = state_3 |> drop Red 1 |> drop Yellow 1 |> drop Yellow 2
              |> drop Red 3 |> drop Red 3 |> drop Yellow 3

(* The state following state_3 that makes red win with a diagonal 4.
   5 Y E E E E E E
   4 R E E E E E E
   3 Y E E R E E E
   2 R E E Y R E E
   1 R E R R Y R E
   0 Y E Y Y R Y R
     0 1 2 3 4 5 6
*)
let state_7 = state_3 |> drop Yellow 5 |> drop Red 5 |> drop Red 4
              |> drop Yellow 4 |> drop Red 4 |> drop Red 3 |> drop Yellow 3
              |> drop Red 3

(* The state following state_3 that makes yellow win with a diagonal 4.
   5 Y E E E E E Y
   4 R E E E E Y R
   3 Y E E E Y Y Y
   2 R E E Y R Y R
   1 R E R Y R R Y
   0 Y E Y Y R R R
     0 1 2 3 4 5 6
*)
let state_8 = state_3 |> drop Yellow 6 |> drop Red 6 |> drop Yellow 6
              |> drop Red 6 |> drop Yellow 6 |> drop Red 5 |> drop Red 5
              |> drop Yellow 5 |> drop Yellow 5 |> drop Yellow 5 |> drop Red 4
              |> drop Red 4 |> drop Red 4 |> drop Yellow 4 |> drop Yellow 3
              |> drop Yellow 3

(* The state following state_3 that makes yellow win with a diagonal 4.
   5 Y E E E E E E
   4 R Y E E E E E
   3 Y R Y E E E E
   2 R R Y Y E E E
   1 R Y R R E E E
   0 Y R Y Y E E R
     0 1 2 3 4 5 6
*)
let state_9 = state_3 |> drop Red 1 |> drop Yellow 1 |> drop Red 1 |> drop Red 1
              |> drop Yellow 1 |> drop Yellow 2 |> drop Yellow 2 |> drop Red 3 
              |> drop Yellow 3 

(* The state following start_state after dropping a few more tokens. 
   A tie state.
   5 R R R Y R R R
   4 Y Y Y R Y Y Y
   3 R R R Y R R R
   2 Y Y Y R Y Y Y
   1 R R R Y R R R
   0 Y Y Y R Y Y Y
     0 1 2 3 4 5 6
*)
let state_10 = start_state |> drop Yellow 0 |> drop Red 0 |> drop Yellow 0 
               |> drop Red 0 |> drop Yellow 0 |> drop Red 0 |> drop Yellow 2 
               |> drop Red 2 |> drop Yellow 2 |> drop Red 2 |> drop Yellow 2 
               |> drop Red 2 |> drop Yellow 1 |> drop Red 1 |> drop Yellow 1 
               |> drop Red 1 |> drop Yellow 1 |> drop Red 1 |> drop Yellow 4 
               |> drop Red 3 |>  drop Yellow 3 |> drop Red 3 |> drop Yellow 3 
               |> drop Red 3 |> drop Yellow 3 |> drop Red 4 |> drop Yellow 4 
               |> drop Red 4 |> drop Yellow 4 |> drop Red 4 |> drop Yellow 5 
               |> drop Red 5 |> drop Yellow 5 |> drop Red 5 |> drop Yellow 5 
               |> drop Red 5 |> drop Yellow 6 |> drop Red 6 |> drop Yellow 6 
               |> drop Red 6 |> drop Yellow 6 |> drop Red 6

(********************************************************************
   End of definitions for testing State. 
 ********************************************************************)

(********************************************************************
   Here are some functions that create tests for State. 
 ********************************************************************)

(** [state_get_token_test name r_i c_i t expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] 
    with [get_token c_i r_i t]. *)
let state_get_token_test 
    (name : string)
    (c_i : int)
    (r_i : int)
    (t : t)
    (expected_output : token) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_token c_i r_i t))

(** [state_get_turn_test name t expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] 
    with [get_turn t]. *)
let state_get_turn_test 
    (name : string)
    (t : t)
    (expected_output : turn) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_turn t))

(** [state_get_board_test name t expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] 
    with [get_board t]. *)
let state_get_board_test 
    (name : string)
    (t : t)
    (expected_output : token list list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_board t))

let state_drop_test
    (name : string)
    (token : token)
    (c_i: int)
    (t : t)
    (expected_output : t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (drop token c_i t))   

(********************************************************************
   End of functions that create tests for State. 
 ********************************************************************)

let state_tests = [
  (* start_state *)
  state_get_token_test "start_state 0 0" 0 0 start_state Empty;
  state_get_token_test "start_state 0 1" 0 1 start_state Empty;
  state_get_token_test "start_state 6 0" 6 0 start_state Empty;
  state_get_token_test "start_state 6 5" 6 5 start_state Empty;
  state_get_token_test "start_state 3 3" 3 3 start_state Empty;
  state_get_turn_test "start_state's default turn is YellowTurn" 
    start_state YellowTurn;
  state_get_board_test "state_get_board start_state" start_state 
    ([
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
    ]);

  (* state_1 *)
  state_get_token_test  "state_1 0 0" 0 0 state_1 Yellow;
  state_get_token_test  "state_1 0 1" 0 1 state_1 Empty;
  state_get_token_test  "state_1 1 1" 2 2 state_1 Empty;
  state_get_token_test  "state_1 1 0" 5 3 state_1 Empty;
  state_get_turn_test "state_1's turn is RedTurn" state_1 RedTurn;
  state_get_board_test "state_get_board state_1" state_1 
    ([
      [Yellow;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
    ]);

  (* state_2 *)
  state_get_token_test  "state_2 0 0" 0 0 state_2 Yellow;
  state_get_token_test  "state_2 0 1" 0 1 state_2 Red;
  state_get_token_test  "state_2 1 0" 0 2 state_2 Empty;
  state_get_token_test  "state_2 2 0" 0 3 state_2 Empty;
  state_get_token_test  "state_2 2 0" 0 4 state_2 Empty;
  state_get_token_test  "state_2 0 5" 0 5 state_2 Empty;
  state_get_token_test  "state_2 6 5" 6 0 state_2 Empty;
  state_get_turn_test "state_2's turn is YellowTurn" state_2 YellowTurn;
  state_get_board_test "state_get_board state_2" state_2 
    ([
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
    ]);

  (* state_3 *)
  state_get_token_test  "state_3 0 0" 0 0 state_3 Yellow;
  state_get_token_test  "state_3 0 1" 0 1 state_3 Red;
  state_get_token_test  "state_3 0 2" 0 2 state_3 Red;
  state_get_token_test  "state_3 0 3" 0 3 state_3 Yellow;
  state_get_token_test  "state_3 0 4" 0 4 state_3 Red;
  state_get_token_test  "state_3 0 5" 0 5 state_3 Yellow;
  state_get_token_test  "state_3 1 0" 1 0 state_3 Empty;
  state_get_token_test  "state_3 2 0" 2 0 state_3 Yellow;
  state_get_token_test  "state_3 2 1" 2 1 state_3 Red;
  state_get_token_test  "state_3 3 1" 3 0 state_3 Yellow; 
  state_get_token_test  "state_3 4 0" 4 0 state_3 Empty; 
  state_get_token_test  "state_3 5 0" 5 0 state_3 Empty; 
  state_get_token_test  "state_3 6 0" 6 0 state_3 Red; 
  state_get_turn_test "state_3's turn is YellowTurn" state_3 YellowTurn;
  state_get_board_test "state_get_board state_3" state_3 
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  (* vertical win conditions *)
  state_get_turn_test "state_4's turn is RedWins" state_4 RedWins;
  state_get_board_test "state_get_board state_4" state_4 
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Red;Red;Red;Empty];
      [Yellow;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  state_get_turn_test "state_4''s turn is RedWins" state_4' YellowWins;
  state_get_board_test "state_get_board state_4" state_4' 
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Yellow;Yellow;Yellow;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  (* horizontal win conditions *)
  state_get_turn_test "state_5's turn is YellowWins" state_5 YellowWins;
  state_get_board_test "state_get_board state_5" state_5
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Yellow;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  state_get_turn_test "state_5''s turn is RedWins" state_5' RedWins;
  state_get_board_test "state_get_board state_5'" state_5'
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Red;Red;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]); 
  (* diagonal win conditions *)
  state_get_turn_test "state_6's turn is YellowWins" state_6 YellowWins;
  state_get_board_test "state_get_board state_6" state_6
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Red;Yellow;Empty;Empty;Empty;Empty];
      [Yellow;Red;Yellow;Empty;Empty;Empty];
      [Yellow;Red;Red;Yellow;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  state_get_turn_test "state_7's turn is RedWins" state_7 RedWins;
  state_get_board_test "state_get_board state_7" state_7
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Red;Yellow;Red;Empty;Empty];
      [Red;Yellow;Red;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);
  state_get_turn_test "state_8's turn is YellowWins" state_8 YellowWins;
  state_get_board_test "state_get_board state_8" state_8
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Yellow;Red;Empty;Empty;Empty;Empty];
      [Yellow;Yellow;Yellow;Empty;Empty;Empty];
      [Red;Red;Red;Yellow;Empty;Empty];
      [Red;Red;Yellow;Yellow;Yellow;Empty];
      [Red;Yellow;Red;Yellow;Red;Yellow];
    ]);
  state_get_turn_test "state_9's turn is YellowWins" state_9 YellowWins;
  state_get_board_test "state_get_board state_9" state_9
    ([
      [Yellow;Red;Red;Yellow;Red;Yellow];
      [Red;Yellow;Red;Red;Yellow;Empty];
      [Yellow;Red;Yellow;Yellow;Empty;Empty];
      [Yellow;Red;Yellow;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Empty;Empty;Empty;Empty;Empty;Empty];
      [Red;Empty;Empty;Empty;Empty;Empty];
    ]);

  (* tie conditions/full board *)
  state_get_turn_test "state_10's turn is Tie" state_10 Tie;
  state_get_board_test "state_get_board state_10" state_10
    ([
      [Yellow;Red;Yellow;Red;Yellow;Red];
      [Yellow;Red;Yellow;Red;Yellow;Red];
      [Yellow;Red;Yellow;Red;Yellow;Red];
      [Red;Yellow;Red;Yellow;Red;Yellow];
      [Yellow;Red;Yellow;Red;Yellow;Red];
      [Yellow;Red;Yellow;Red;Yellow;Red];
      [Yellow;Red;Yellow;Red;Yellow;Red];
    ]);

  (* edge cases - exceptions *)
  "drop in full column" >:: (fun _ ->
      assert_raises (FullCol) (fun () -> drop Yellow 0 state_3));
  "drop in full board" >:: (fun _ ->
      assert_raises (FullCol) (fun () -> drop Red 1 state_10));    
  "drop in invalid column" >:: (fun _ ->
      assert_raises (InvalidCol) (fun () -> drop Yellow (-1) start_state));
  "drop in invalid column" >:: (fun _ ->
      assert_raises (InvalidCol) (fun () -> drop Yellow 7 start_state));           
]

(*********************
   Below are tests for part of the Window module and a function to make
   those tests.
 **********************)

(** [window_within_test name t expected_output] constructs an OUnit
    test named [name] that asserts the equality of [expected_output] 
    with [ t]. *)
let window_within_test 
    (name : string)
    (t : bool) = 
  name >:: (fun _ -> assert_bool "within failure" t)

(** [pos_window] is a window located in the positive section of the
    coordinate system *)
let pos_window : Window.region = 
  {x = 0.5; y = 0.5; width = 0.5; height = 0.5}

(** [neg_window] is a window located in the negative section of the
    coordinate system *)
let neg_window : Window.region = 
  {x = -0.5; y = -0.5; width = 0.5; height = 0.5}
let neg_window: Window.region = {x = -0.5; y = -0.5; width = 0.5; height = 0.5}

let window_tests = [
  window_within_test "point at center"
    (Window.within pos_window (0.5, 0.5));

  window_within_test "point above" 
    (not (Window.within pos_window (0.5, 0.8)));

  window_within_test "point below" 
    (not (Window.within pos_window (0.5, 0.2)));

  window_within_test "point left" 
    (not (Window.within pos_window (0.2, 0.5)));

  window_within_test "point right" 
    (not (Window.within pos_window (0.8, 0.5)));

  window_within_test "point negative" 
    (not (Window.within pos_window (-0.5, -0.5)));

  (* does this work even when region center is negative? *)
  window_within_test "point at center neg"
    (Window.within neg_window (-0.5, -0.5));

  window_within_test "point above neg" 
    (not (Window.within neg_window (-0.5, -0.2)));

  window_within_test "point below neg" 
    (not (Window.within neg_window (-0.5, -0.8)));

  window_within_test "point left neg" 
    (not (Window.within neg_window (-0.8, -0.5)));

  window_within_test "point right neg" 
    (not (Window.within neg_window (-0.2, 0.5)));

  window_within_test "point positive neg" 
    (not (Window.within neg_window (0.5, 0.5)));

]

(* the test suite *)
let suite =
  "test suite for connect_four"  >::: List.flatten [
    state_tests;
    window_tests;
  ]

(* run the test suite *)
let _ = run_test_tt_main suite