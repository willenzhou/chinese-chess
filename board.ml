open Piece

type board = (Piece.t) list

let chuteam = 
  [piece_of_tuple (Chariot, Chu, (0,0));
   piece_of_tuple (Horse, Chu, (0,1));
   piece_of_tuple (Elephant, Chu, (0,2));
   piece_of_tuple (Guard, Chu, (0,3));
   piece_of_tuple (General, Chu, (0,4));
   piece_of_tuple (Guard, Chu, (0,5));
   piece_of_tuple (Elephant, Chu, (0,6));
   piece_of_tuple (Horse, Chu, (0,7));
   piece_of_tuple (Chariot, Chu, (0,8));
   piece_of_tuple (Cannon, Chu, (2,1));
   piece_of_tuple (Cannon, Chu, (2,7));
   piece_of_tuple (Soldier, Chu, (3,0));
   piece_of_tuple (Soldier, Chu, (3,2));
   piece_of_tuple (Soldier, Chu, (3,4));
   piece_of_tuple (Soldier, Chu, (3,6));
   piece_of_tuple (Soldier, Chu, (3,8));]

let hanteam = [
  piece_of_tuple (Soldier, Han, (6,0)); 
  piece_of_tuple (Soldier, Han, (6,2)); 
  piece_of_tuple (Soldier, Han, (6,4)); 
  piece_of_tuple (Soldier, Han, (6,6));
  piece_of_tuple (Soldier, Han, (6,8)); 
  piece_of_tuple (Cannon, Han, (7,1));  
  piece_of_tuple (Cannon, Han, (7,7)); 
  piece_of_tuple (Chariot, Han, (9,0)); 
  piece_of_tuple (Horse, Han, (9,1)); 
  piece_of_tuple (Elephant, Han, (9,2)); 
  piece_of_tuple (Guard, Han, (9,3)); 
  piece_of_tuple (General, Han, (9,4)); 
  piece_of_tuple (Guard, Han, (9,5)); 
  piece_of_tuple (Elephant, Han, (9,6)); 
  piece_of_tuple (Horse, Han, (9,7)); 
  piece_of_tuple (Chariot, Han, (9,8)); 
]

let initial_board = chuteam @ hanteam

let empty_board = []

(** List of all positions on the board. *)
let boardpos_list = 
  ["00"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08";
   "newline"; "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18";
   "newline"; "20"; "21"; "22"; "23"; "24"; "25"; "26"; "27"; "28";
   "newline"; "30"; "31"; "32"; "33"; "34"; "35"; "36"; "37"; "38";
   "newline"; "40"; "41"; "42"; "43"; "44"; "45"; "46"; "47"; "48";
   "newline"; "river"; "river"; "river"; "river"; "river"; 
   "river"; "river"; "river"; "river";
   "newline"; "50"; "51"; "52"; "53"; "54"; "55"; "56"; "57"; "58";
   "newline"; "60"; "61"; "62"; "63"; "64"; "65"; "66"; "67"; "68";
   "newline"; "70"; "71"; "72"; "73"; "74"; "75"; "76"; "77"; "78";
   "newline"; "80"; "81"; "82"; "83"; "84"; "85"; "86"; "87"; "88";
   "newline"; "90"; "91"; "92"; "93"; "94"; "95"; "96"; "97"; "98"
  ]

(** [string_of_genre piece] converts piece's genre to string for printing. *)
let string_of_genre (piece : Piece.t) : string = 
  if get_team piece = Chu then 
    match get_genre piece with 
    | Soldier -> "卒"
    | Cannon -> "砲"
    | Chariot -> "車"
    | Horse -> "馬"
    | Elephant -> "象"
    | Guard -> "士"
    | General -> "將"
  else 
    match get_genre piece with 
    | Soldier -> "兵"
    | Cannon -> "炮"
    | Chariot -> "俥"
    | Horse -> "傌"
    | Elephant -> "相"
    | Guard -> "仕"
    | General -> "帥"

(** [string_of_position piece] adds two numbers in piece's 
    coordinates together. *)
let string_of_position (piece : Piece.t) : string = 
  match get_pos piece with 
  | (x,y) -> (string_of_int x) ^ (string_of_int y)

(** Prints the piece to the terminal. *)
let print_piece (piece : Piece.t) : unit = 
  print_string (string_of_genre piece)

(** [position_exist location piece] tests 
    to see if the [piece] is at [location]*)
let position_exist (location : string) piece = 
  if location = string_of_position piece then [piece]
  else []

(** [match_pos board location] serves as a go-between for [piece_exist_checker] 
    and [position_exist]. *)
let rec match_pos board (location : string) = 
  match board with 
  | [] -> []
  | h :: t -> position_exist location h @ match_pos t location

(** [piece_exist_checker board location] checks 
    if there is a piece at the location,
    if there is, it is printed, and if not, 
    an empty space is printed. *)
let piece_exist_checker board (location : string) = 
  let truelist = match_pos board location in 
  match truelist with 
  | [] -> print_string "  "
  | h :: t -> if List.length truelist != 0 then print_piece h

(** [compare_pos board location] checks if [location] is a river 
    or the beginning of a new line. Then it transfers to 
    to [comparepos_helper location lastpiece piece]. *)
let compare_pos board (location : string) : unit = 
  if location = "river" then print_string "河"
  else if location = "newline" then print_string "\n"
  else piece_exist_checker board location

let paint board = 
  List.iter (compare_pos board) boardpos_list 

exception InvalidPosition of (int * int)

let rec get_piece pos = function 
  | [] -> raise (InvalidPosition pos)
  | h :: t -> if get_pos h = pos then h else get_piece pos t

let rec remove pos = function
  | [] -> []
  | h :: t -> if get_pos h = pos then t else [h] @ remove pos t 