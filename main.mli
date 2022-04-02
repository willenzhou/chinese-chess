open Board
open Piece 
open Move 
open Command

(**
    The main interface for the Chinese Chess game.
*)

(** Raised when state changes. *)
exception StateChange of string 

(** Raised an attempt is made to move a piece on the wrong team. *)
exception IncorrectTeam of string

(** [process_command command] processes the [command]. *)
val process_command: command -> board -> team -> int -> bool -> board