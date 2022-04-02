open Piece

(**
    Representation of Chess Move.
*)

(** The type representing a move on the chess board. *)
type move = Piece.t * (int * int)

(** [check_move piece (x, y) board] 
    Is true if a move is valid given a piece and its target position, 
    and is false otherwise. *)
val check_move: Piece.t -> (int * int) -> (Piece.t list) -> bool

(** [check_lose board team] 
    Is true if the team [team] has lost the game and is false when 
    [team] hasn't lost yet. *)
val check_lose: (Piece.t list) -> team -> bool