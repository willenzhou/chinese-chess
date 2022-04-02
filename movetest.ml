open OUnit2
open Piece
open Move

let move_test 
    (name : string) 
    (piece: Piece.t) 
    (pos2: (int * int)) board
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_move piece pos2 board)
        ~printer:string_of_bool)


let horse1 = piece_of_tuple (Horse, Han, (9,1))
let horse2 = piece_of_tuple (Horse, Han, (9,2))
let horseboard = [
  horse1;
  horse2;
]
let soldier1 = piece_of_tuple (Soldier, Chu, (0,0))
let soldier2 = piece_of_tuple (Soldier, Han, (1,1))


let soldier_tests = [
  move_test "out of bound" soldier1 (0,-1) [] false;
  move_test "legit Chu move" soldier1 (1,0) [] true;
  move_test "illegal Chu move" soldier1 (1,1) [] false;
  move_test "moved too much" soldier1 (2, 0) [] false;
  move_test "legit Han move vert" soldier2 (0,1) [] true;
  move_test "legit Han move hori" soldier2 (1,2) [] true;
  move_test "legit Han move hori2" soldier2 (1,0) [] true;
  move_test "moved too much" soldier2 (2, 2) [] false;
]

let horse_tests = [
  move_test "legit move up" horse2 (8, 4) horseboard true;
  move_test "legit move up left" horse1 (7, 2) horseboard true;
  move_test "legit move up left 2" horse2 (7, 3) horseboard true;
  move_test "illegit move" horse2 (7,4) horseboard false;
  move_test "illegit move bc stuck" horse1 (8,3) horseboard false;
]

let elephant1 = piece_of_tuple(Elephant, Chu, (0, 2))
let block = piece_of_tuple(Elephant, Chu, (1,1))
let elephant2 = piece_of_tuple(Elephant, Han, (9, 2))

let elephant_board = [
  elephant1;
  elephant2;
  block
]

let elephant_tests = [
  move_test "legit move up" elephant2 (7, 4) elephant_board true;
  move_test "legit move up" elephant2 (7, 0) elephant_board true;
  move_test "legit move up" elephant1 (2, 4) elephant_board true;
  move_test "blocked" elephant1 (2, 0) elephant_board false;
]

let chariot1 = piece_of_tuple(Chariot, Chu, (0,0))
let chariot2 = piece_of_tuple(Chariot, Chu, (6,0))
let chariot_board = [
  chariot1;
  chariot2;
]

let chariot_tests = [
  move_test "legit move right" chariot1 (0, 5) chariot_board true;
  move_test "legit move down" chariot1 (5, 0) chariot_board true;
  move_test "blocked path" chariot1 (7, 0) chariot_board false;
]

let cannon1 = piece_of_tuple(Cannon, Chu, (0,0))
let cannon2 = piece_of_tuple(Cannon, Chu, (6,0))
let cannon3 = piece_of_tuple(Cannon, Han, (7,0))
let cannon_board = [
  cannon1;
  cannon2;
  cannon3
]
let cannon_tests = [
  move_test "legit move right" cannon1 (0, 5) cannon_board true;
  move_test "legit move right" cannon2 (6, 5) cannon_board true;
  move_test "legit move down" cannon1 (5, 0) cannon_board true;
  move_test "eat!!" cannon1 (7, 0) cannon_board true;
  move_test "GO past 2" cannon1 (8, 0) cannon_board false;
]

let tests = List.flatten [
    soldier_tests;
    horse_tests;
    elephant_tests;
    chariot_tests;
    cannon_tests;
  ]

