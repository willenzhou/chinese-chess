(**
   Representation of Chess Piece.
*)

(** The abstract type of values representing the chess piece. *)
type t

(** The abstract type representing the genre of the chess piece. *)
type genre = Soldier | Cannon | Horse | Elephant | Guard | General | Chariot

(** The abstract type representing the team of the chess piece. *)
type team = Han | Chu

(** Returns the genre of the chess piece. *)
val get_genre: t -> genre

(** Returns the team the chess piece belongs to. *)
val get_team: t -> team

(** Returns the position of the chess piece. *)
val get_pos: t -> (int * int)

(** Returns a chess piece, given a tuple on its information.
    Example: [piece_of_tuple (King Han (2,2))] *)
val piece_of_tuple: (genre * team * (int * int)) -> t
