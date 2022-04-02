(** 
   Representation of Chinese Chess board.
*)

(** The abstract type of values representing the chess board. *)
type board = (Piece.t) list

(** Returns initial board setup. *)
val initial_board: board

(** List of pieces on Chu team. *)
val chuteam: board

(** List of pieces on Han team. *)
val hanteam: board

(** Returns an empty board. *)
val empty_board: board

(** Draws out the current board in the terminal. *)
val paint: board -> unit

(** Raised when there is no piece at (int * int) *)
exception InvalidPosition of (int * int)

(** Returns piece at (int * int) from the board. *)
val get_piece: (int * int) -> board -> Piece.t

(**  Removes the piece at (int * int) from the board. *)
val remove: (int * int) -> board -> board

