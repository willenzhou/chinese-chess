open OUnit2 
open Board 
open Piece 
open Move
open Command

let parse_test
    (name : string)
    (input : string)
    (output : command) : test =
  name >:: (fun _ ->
      assert_equal output (parse input))

let invalidparse_test
    (name : string)
    (input : string)
    (output : string) : test =
  name >:: (fun _ ->
      assert_raises 
        (InvalidCommand output) (fun () -> parse input))

let validcommand_tests = [
  parse_test "Player test" "player" Player;
  parse_test "Player test with spaces" "  player " Player;
  parse_test "easy Ai test" "easyai" (Ai "easy");
  parse_test "easy Ai test with spaces" "  easyai " (Ai "easy");
  parse_test "medium Ai test" "mediumai" (Ai "medium");
  parse_test "medium Ai test with spaces" "  mediumai " (Ai "medium");
  parse_test "hard Ai test" "hardai" (Ai "hard");
  parse_test "hard Ai test with spaces" "  hardai " (Ai "hard");
  parse_test "Quit test" "quit" Quit;
  parse_test "Quit test with spaces" " quit " Quit;
  parse_test "Learn test" "learn" Learn;
  parse_test "Learn test with spaces" " learn " Learn;
  parse_test "Move test" "move 8 3 4 2" (Move ((8,3), (4,2)));
  parse_test "Move test 2" "move 1 5 6 7" (Move ((1,5), (6,7)));
  parse_test "Move test 3" "move 0 0 9 9" (Move ((0,0), (9,9)));
  parse_test "PieceName test: general" "general" (PieceName "general");
  parse_test "PieceName test with spaces: general" " general " 
    (PieceName "general");
  parse_test "PieceName test: guard" "guard" (PieceName "guard");
  parse_test "PieceName test with spaces: guard" " guard " 
    (PieceName "guard");
  parse_test "PieceName test: elephant" "elephant" (PieceName "elephant");
  parse_test "PieceName test with spaces: elephant" " elephant " 
    (PieceName "elephant");
  parse_test "PieceName test: horse" "horse" (PieceName "horse");
  parse_test "PieceName test with spaces: horse" " horse " 
    (PieceName "horse");
  parse_test "PieceName test: chariot" "chariot" (PieceName "chariot");
  parse_test "PieceName test with spaces: chariot" " chariot " 
    (PieceName "chariot");
  parse_test "PieceName test: cannon" "cannon" (PieceName "cannon");
  parse_test "PieceName test with spaces: cannon" " cannon " 
    (PieceName "cannon");
  parse_test "PieceName test: soldier" "soldier" (PieceName "soldier");
  parse_test "PieceName test with spaces: soldier" " soldier " 
    (PieceName "soldier");
  parse_test "Puzzle test: puzzle 1" "puzzle 1" (Puzzle "1");
  parse_test "Puzzle test with spaces: puzzle 1" " puzzle 1 " 
    (Puzzle "1");
  parse_test "Puzzle test: puzzle 2" "puzzle 2" (Puzzle "2");
  parse_test "Puzzle test with spaces: puzzle 2" " puzzle 2 " 
    (Puzzle "2");
  parse_test "Puzzle test: puzzle 3" "puzzle 3" (Puzzle "3");
  parse_test "Puzzle test with spaces: puzzle 3" " puzzle 3 " 
    (Puzzle "3");
  parse_test "Puzzle test: puzzle 4" "puzzle 4" (Puzzle "4");
  parse_test "Puzzle test with spaces: puzzle 4" " puzzle 4 " 
    (Puzzle "4");
  parse_test "Puzzle test: puzzle 5" "puzzle 5" (Puzzle "5");
  parse_test "Puzzle test with spaces: puzzle 5" " puzzle 5 " 
    (Puzzle "5");
  parse_test "Puzzle test: puzzle 6" "puzzle 6" (Puzzle "6");
  parse_test "Puzzle test with spaces: puzzle 6" " puzzle 6 " 
    (Puzzle "6");
  parse_test "Puzzle test: puzzle 7" "puzzle 7" (Puzzle "7");
  parse_test "Puzzle test with spaces: puzzle 7" " puzzle 7 " 
    (Puzzle "7");
  parse_test "Puzzle test: puzzle 8" "puzzle 8" (Puzzle "8");
  parse_test "Puzzle test with spaces: puzzle 8" " puzzle 8 " 
    (Puzzle "8");
  parse_test "Puzzle test: puzzle 9" "puzzle 9" (Puzzle "9");
  parse_test "Puzzle test with spaces: puzzle 9" " puzzle 9 " 
    (Puzzle "9");
]

let invalidcommand_tests = [
  invalidparse_test "Player test: incorrect verb" 
    "playerr" "Malformed command, try again.";
  invalidparse_test "Player test: extra stuff after verb"
    "player bob" "Malformed command, try again.";
  invalidparse_test "Ai test: incorrect verb" 
    "aii" "Malformed command, try again.";
  invalidparse_test "Ai test: extra stuff after verb"
    "easyai superchess" "Malformed command, try again.";
  invalidparse_test "Quit test: incorrect verb" 
    "quitt" "Malformed command, try again.";
  invalidparse_test "Quit test: extra stuff after verb"
    "quit hello" "Malformed command, try again.";
  invalidparse_test "Learn test: incorrect verb" 
    "learnn" "Malformed command, try again.";
  invalidparse_test "Learn test: extra stuff after verb"
    "learn hello" "Malformed command, try again.";
  invalidparse_test "Empty command" "" "Empty command, try again.";
  invalidparse_test "Move test: incorrect verb" 
    "movee" "Malformed command, try again.";
  invalidparse_test "Move test: wrong stuff after verb" 
    "move h e l l" "Incorrect move parameters type, try again.";
  invalidparse_test "Move test: wrong stuff after verb 2" 
    "move 3 1 5 p" "Incorrect move parameters type, try again.";
  invalidparse_test "Move test: nonexistent coordinates" 
    "move 3 1 5 100" "Incorrect move parameters type, try again.";
  invalidparse_test "Move test: nothing after verb" 
    "move " "Malformed command, try again.";
  invalidparse_test "Move test: missing 1 coordinate" 
    "move 8 3 4" "Malformed command, try again.";
  invalidparse_test "Move test: missing 2 coordinates" 
    "move 8 4" "Malformed command, try again.";
  invalidparse_test "Move test: missing 3 coordinates" 
    "move 8" "Malformed command, try again.";
]


let tests =
  List.flatten [
    validcommand_tests;
    invalidcommand_tests;
  ]

