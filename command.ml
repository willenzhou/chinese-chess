open Board
open Piece 
open Move 

type coord = (int * int)

type command = 
  | Player
  | Ai of string
  | Learn
  | PieceName of string
  | Puzzle of string
  | Quit 
  | Move of (coord * coord)

exception InvalidCommand of string

(** [check_int str] checks to see if [str] is a valid number that can
    be used in a coordinate. *)
let rec check_int str = 
  let intstr = int_of_char(String.get str 0) in 
  if intstr >= Char.code '0' && intstr <= Char.code '9' 
     && String.length str = 1
  then str 
  else raise (InvalidCommand "Incorrect move parameters type, try again.")

(** [str_to_charlist lst] converts [lst] 
    from a string list to a character list. *)
let rec str_to_intlist lst = 
  match lst with
  | [] -> [] 
  | h :: t -> [int_of_string (check_int h)] @ str_to_intlist t

(** [process_move lst] returns the [lst] in proper format to execute the
    Move command. *)
let process_move lst = 
  let strlist = str_to_intlist lst in 
  match strlist with 
  | [a; b; c; d] -> ((a,b), (c,d))
  | _ -> raise (InvalidCommand "Malformed move command, try again.")

(** [check_malformed] is a helper function for [parse]. It checks if there is
    a valid command. *)
let rec check_malformed str lst = 
  let lowercase = String.lowercase_ascii str in 
  if lowercase = "" then raise (InvalidCommand "Empty command, try again.")
  else if lowercase = "player" && List.length lst = 0 then Player
  else if lowercase = "easyai" && List.length lst = 0 then Ai "easy"
  else if lowercase = "mediumai" && List.length lst = 0 then Ai "medium"
  else if lowercase = "hardai" && List.length lst = 0 then Ai "hard"
  else if lowercase = "learn" && List.length lst = 0 then Learn
  else if lowercase = "quit" && List.length lst = 0 then Quit
  else if lowercase = "move" && List.length lst = 4 then Move (process_move lst)
  else if 
    lowercase = "soldier" || lowercase = "cannon" || lowercase = "horse"
    || lowercase = "elephant" || lowercase = "guard" || lowercase = "general" ||
    lowercase = "chariot" && List.length lst = 0 then PieceName lowercase
  else if List.length lst = 1 && lowercase = "puzzle" then Puzzle (List.hd lst)
  else raise (InvalidCommand "Malformed command, try again.")

let parse (str : string) : command = 
  let strlist = String.split_on_char ' ' (String.trim str) in 
  match strlist with 
  | [] -> raise (InvalidCommand "Empty command, try again.")
  | h :: t -> check_malformed h t
