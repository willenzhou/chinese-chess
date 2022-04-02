open Board 
open Piece 
open Move 
open Command 

let rec start_learn () = 
  print_string "\n";
  print_string "Welcome to Learn Mode!";
  print_string "\n";
  print_string "Chinese Chess, also known as Xiangqi 象棋, \
                is a popular strategy board game for two players.";
  print_string "\n";
  print_string 
    "Chinese Chess shares many similarities with international Chess.";
  print_string "\n";
  print_string
    "It is a game involving two armies called Chu and Han with a river between \
     the armies.";
  print_string "\n";
  print_string
    "The objective is to capture the opponent's general \
     (corresponds to the king in international chess).";
  print_string "\n";
  print_string "There are 7 types of pieces in the game:";
  print_string "\n";
  print_string "General, Guard, Elephant, Horse, Chariot, Cannon, Soldier";
  print_string "\n";
  print_string "Enter the name of the piece you want to learn more about: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [find_piece comm] processes the command [comm]. *)
and find_piece (comm : command) = 
  match comm with 
  | PieceName "soldier" -> soldier_tutorial ()
  | PieceName "cannon" -> cannon_tutorial ()
  | PieceName "horse" -> horse_tutorial ()
  | PieceName "elephant" -> elephant_tutorial ()
  | PieceName "guard" -> guard_tutorial ()
  | PieceName "general" -> general_tutorial ()
  | PieceName "chariot" -> chariot_tutorial ()
  | Quit -> exit 0
  |  _ -> print_string "\n Invalid command, try again. \n" ; start_learn ()

(** [soldier_tutorial] is a brief tutorial on the soldier piece. *)
and soldier_tutorial () = 
  print_string "\n";
  print_string "Soldiers are displayed as 卒 on the Chu team \
                and 兵 on the Han team.";
  print_string "\n";
  print_string "They move and capture by moving one point forward, but cannot \
                move backward.";
  print_string "\n";
  print_string "After crossing the river, they can also move and capture one \
                point horizontally.";
  print_string "\n";
  print_string "Once they reach the edge, they will only be able to move \
                sideways at the enemy's edge.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [cannon_tutorial] is a brief tutorial on the cannon piece. *)
and cannon_tutorial () = 
  print_string "\n";
  print_string "Cannons are displayed as 砲 on the Chu team \
                and 炮 on the Han team.";
  print_string "\n";
  print_string "They can move any distance orthogonally without jumping.";
  print_string "\n";
  print_string "But, capture requires jumping over a single piece \
                (team does not matter) on the path of attack.";
  print_string "\n";
  print_string "The number of spaces between the cannon and the piece to be \
                captured does not matter.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [horse_tutorial] is a brief tutorial on the horse piece. *)
and horse_tutorial () = 
  print_string "\n";
  print_string "Horses are displayed as 馬 on the Chu team \
                and 傌 on the Han team.";
  print_string "\n";
  print_string "They move and capture one point orthogonally and then one \
                point diagonally from its position.";
  print_string "\n";
  print_string "The horse does not jump like the knight in international \
                Chess.";
  print_string "\n";
  print_string "Therefore, the horse can be blocked by a piece located \
                one point horizontally or vertically adjacent to it.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [elephant_tutorial] is a brief tutorial on the elephant piece. *)
and elephant_tutorial () = 
  print_string "\n";
  print_string "Elephants are displayed as 象 on the Chu team \
                and 相 on the Han team.";
  print_string "\n";
  print_string "They move and capture two points diagonally and may not \
                jump over pieces.";
  print_string "\n";
  print_string "The elephant cannot cross the river.";
  print_string "\n";
  print_string "Therefore, the elephant can be blocked easily, so they mostly \
                serve as defensive pieces.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [guard_tutorial] is a brief tutorial on the guard piece. *)
and guard_tutorial () = 
  print_string "\n";
  print_string "Guards are displayed as 士 on the Chu team \
                and 仕 on the Han team.";
  print_string "\n";
  print_string "They move and capture one point diagonally.";
  print_string "\n";
  print_string "The guards are restricted to the palace, so they are confined \
                to 5 points on the board.";
  print_string "\n";
  print_string "Therefore, they serve to protect the general.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [general_tutorial] is a brief tutorial on the general piece. *)
and general_tutorial () = 
  print_string "\n";
  print_string "Generals are displayed as 將 on the Chu team \
                and 帥 on the Han team.";
  print_string "\n";
  print_string "They move and capture one point orthogonally.";
  print_string "\n";
  print_string "The general may not leave the palace.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)

(** [chariot_tutorial] is a brief tutorial on the chariot piece. *)
and chariot_tutorial () = 
  print_string "\n";
  print_string "Chariots are displayed as 車 on the Chu team \
                and 俥 on the Han team.";
  print_string "\n";
  print_string "They move and capture any distance orthogonally.";
  print_string "\n";
  print_string "They may not jump over pieces.";
  print_string "\n";
  print_string "It is often considered the strongest piece.";
  print_string "\n";
  print_string "Enter another piece name or quit to exit: ";
  let player_input = read_line() in 
  find_piece (parse player_input)