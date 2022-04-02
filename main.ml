open Board
open Piece 
open Move 
open Command
open Ai
open Learn
open Puzzle

exception StateChange of string 

exception IncorrectTeam of string

(** [check_team piece pteam] checks if the team of [piece] is equal to 
    [pteam]. *)
let check_team (piece : Piece.t) (pteam : team) : team =
  if get_team piece = pteam then pteam 
  else raise (IncorrectTeam "Wrong team, try again.")

(** [board_check outputboard] checks if [outputboard] is a puzzle board. *)
let board_check (outputboard: board) : board = 
  if outputboard = puzzle1 || outputboard = puzzle2 || outputboard = puzzle3 || 
     outputboard = puzzle4 || outputboard = puzzle5 || outputboard = puzzle6 || 
     outputboard = puzzle7 || outputboard = puzzle8 || outputboard = puzzle9 
  then raise (StateChange "Puzzle is not a move")
  else outputboard

(** [check_state outputboard] checks to see if the player wants to quit the
    game. *)
let check_state (outputboard : board) (puzzlebool : bool) : board = 
  if outputboard = empty_board then exit 0
  else if outputboard = initial_board then 
    raise (StateChange "Player is not a move")
  else if outputboard = hanteam then 
    raise (StateChange "Learn is not a move")
  else if outputboard = chuteam || 
          outputboard = [piece_of_tuple (Horse, Chu, (0,1))] || 
          outputboard = [piece_of_tuple (Chariot, Chu, (0,0))] 
  then raise (StateChange "Ai is not a move")
  else  board_check outputboard
(* if puzzlebool = false then
   else outputboard *)

(** [switch_team team] switches the [team]. *)
let switch_team (team : team) : team = 
  if team = Chu then Han 
  else Chu

(** [string_of_team team] returns the string of [team]. *)
let string_of_team (team: team) : string = 
  if team = Chu then "Chu Team, "
  else "Han Team, "

(** [move_piece piece coord board] moves the [piece] to the [coord] on
    [board]. *)
let rec move_piece (piece : Piece.t) 
    (coord : coord) (board : board) (team : team) (movenum : int) 
    (puzzlebool : bool) : board = 
  try
    let piece_genre = get_genre piece in 
    let piece_team = check_team piece team in 
    let piece_oldpos = get_pos piece in 
    let piece_pos = coord in 
    let eat_pieceboard = Board.remove piece_pos board in 
    let removeorig_board = Board.remove piece_oldpos eat_pieceboard in
    removeorig_board @ [piece_of_tuple (piece_genre, piece_team, piece_pos)]
  with 
  | IncorrectTeam s -> input_movehelp board team s movenum puzzlebool

(** [easy_ai inputboard team mode movenum puzzlebool] updates the board for
    easyai mode. *)
and easy_ai (inputboard : board) (team : team) (mode : string) 
    (movenum : int) (puzzlebool : bool) = 
  if team = Chu then 
    board_printer (input_movehelp inputboard team "" movenum puzzlebool) 
      (switch_team team) mode (movenum + 1) puzzlebool
  else match (dummy_ai inputboard team) with 
    | (piece, newcoord) -> 
      let newboard = 
        move_piece piece newcoord inputboard team movenum puzzlebool in 
      board_printer 
        (input_movehelp newboard (switch_team team) "" movenum puzzlebool)
        team mode (movenum + 1) puzzlebool

(** [medium_ai inputboard team mode movenum puzzlebool] updates the board for
    mediumai mode. *)
and medium_ai (inputboard : board) (team : team) (mode : string) 
    (movenum : int) (puzzlebool : bool) = 
  if team = Chu then 
    board_printer (input_movehelp inputboard team "" movenum puzzlebool) 
      (switch_team team) mode (movenum + 1) puzzlebool
  else match (smarter_random inputboard team) with 
    | (piece, newcoord) -> 
      let newboard = 
        move_piece piece newcoord inputboard team movenum puzzlebool in 
      board_printer 
        (input_movehelp newboard (switch_team team) "" movenum puzzlebool)
        team mode (movenum + 1) puzzlebool

(** [hard_ai inputboard team mode movenum puzzlebool] updates the board for
    hardai mode. *)
and hard_ai (inputboard : board) (team : team) (mode : string) 
    (movenum : int) (puzzlebool : bool) = 
  if team = Chu then 
    board_printer (input_movehelp inputboard team "" movenum puzzlebool) 
      (switch_team team) mode (movenum + 1) puzzlebool
  else match (minimax_ai inputboard team) with 
    | (piece, newcoord) -> 
      let newboard = 
        move_piece piece newcoord inputboard team movenum puzzlebool in 
      board_printer 
        (input_movehelp newboard (switch_team team) "" movenum puzzlebool)
        team mode (movenum + 1) puzzlebool

(** [board_printer inputboard team mode movenum puzzlebool] updates the board 
    with the player move. *)
and board_printer (inputboard : board) (team : team) (mode : string) 
    (movenum : int) (puzzlebool : bool) = 
  match check_lose inputboard team with 
  | true ->
    print_string (string_of_team (switch_team team) ^ "you have won! \n"); 
    exit 0
  | false ->
    if mode = "easyai" then easy_ai inputboard team mode movenum puzzlebool
    else if mode = "mediumai" then 
      medium_ai inputboard team mode movenum puzzlebool
    else if mode = "hardai" then hard_ai inputboard team mode movenum puzzlebool
    else
      board_printer (input_movehelp inputboard team "" movenum puzzlebool) 
        (switch_team team) mode (movenum + 1) puzzlebool

(** [input_movehelp inputboard] is a recursive helper function for 
    [board_printer]. *)
and input_movehelp (inputboard : board) (team : team) (msg : string) 
    (movenum : int) (puzzlebool : bool) = 
  print_string "\n";
  print_string msg;
  print_string "\n";
  print_string ("Move Number " ^ (string_of_int movenum));
  print_string "\n";
  paint inputboard; 
  print_string "\n"; 
  print_string (string_of_team team ^ "enter your move:");
  print_string "\n";
  let player_input = read_line() in
  try
    let parsed_str = parse player_input in 
    check_state 
      (process_command parsed_str inputboard team movenum puzzlebool) puzzlebool
  with
  | InvalidCommand s -> input_movehelp inputboard team s movenum puzzlebool
  | StateChange "Player is not a move" -> 
    input_movehelp inputboard team "Invalid move, try again." 
      movenum puzzlebool
  | StateChange "Ai is not a move" -> 
    input_movehelp inputboard team "Ai is not a move, try again." 
      movenum puzzlebool
  | StateChange "Learn is not a move" -> 
    input_movehelp inputboard team "Learn is not a move, try again." 
      movenum puzzlebool
  | StateChange "Puzzle is not a move" -> 
    input_movehelp inputboard team "Puzzle is not a move, try again." 
      movenum puzzlebool

(** [puzzle_picker strnum] returns the number puzzle or the initial board if
    not found. *)
and puzzle_picker (strnum : string) = 
  if strnum = "1" then puzzle1 
  else if strnum = "2" then puzzle2 
  else if strnum = "3" then puzzle3
  else if strnum = "4" then puzzle4
  else if strnum = "5" then puzzle5
  else if strnum = "6" then puzzle6
  else if strnum = "7" then puzzle7
  else if strnum = "8" then puzzle8
  else if strnum = "9" then puzzle9
  else initial_board

and process_command (comm : command) (board : board) (team : team) 
    (movenum : int) (puzzlebool : bool) : board  = 
  match comm with 
  | Player -> initial_board
  | Ai d -> if d = "easy" then chuteam 
    else if d = "medium" then [piece_of_tuple (Horse, Chu, (0,1))]
    else [piece_of_tuple (Chariot, Chu, (0,0))]
  | Learn -> hanteam
  | PieceName x -> 
    print_string "Invalid command. \n"; 
    if movenum = 0 then exit 0 else
      input_movehelp board team "Try again." movenum puzzlebool 
  | Puzzle n -> puzzle_picker n
  | Quit -> empty_board
  | Move (oldcoord, newcoord) -> 
    try
      if check_move (get_piece oldcoord board) newcoord board = true 
      then move_piece (get_piece oldcoord board) newcoord board team 
          movenum puzzlebool
      else 
        input_movehelp board team "Move cannot be performed, try again." 
          movenum puzzlebool
    with 
    | InvalidCommand s -> input_movehelp board team s movenum puzzlebool
    | InvalidPosition (x,y) -> 
      input_movehelp board team "Invalid position, try again." 
        movenum puzzlebool

(** [player_start inputstr] determines if the player wants to start the game and
    the game mode. *)
let player_start (inputstr : string) =
  try
    let parsed_str = parse inputstr in 
    let return_board = process_command parsed_str initial_board Chu 0 false in 
    if return_board = empty_board then exit 0
    else if return_board = chuteam then 
      board_printer initial_board Chu "easyai" 1 false 
    else if return_board = [piece_of_tuple (Horse, Chu, (0,1))] then
      board_printer initial_board Chu "mediumai" 1 false 
    else if return_board = [piece_of_tuple (Chariot, Chu, (0,0))] then 
      board_printer initial_board Chu "hardai" 1 false 
    else if return_board = hanteam then start_learn ()
    else if return_board = initial_board then 
      board_printer return_board Chu "" 1 false
    else board_printer return_board Chu "" 1 true
  with 
  | InvalidCommand e -> print_string "Invalid game mode, exiting game."; 
    print_string "\n"; exit 0

(** [main ()] prompts the user for a command. *)
let main () =
  print_string "\n";
  print_string "Welcome to Chinese Chess — OCaml Edition!";
  print_string "\n";
  print_string "Enter player, easyai, mediumai, hardai, learn, \
                or puzzle # (# = 1-9) \
                to determine the game mode:";
  print_string "\n";
  let player_input = read_line() in 
  player_start player_input

(* Execute the game engine. *)
let () = main ()