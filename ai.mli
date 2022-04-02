open Board
open Piece

(**
   Representation of Chess AI.
*)

(** [dummy_ai board piece] 
    Produces a random piece on the board and a random, valid coordinate for the 
    piece to move to. This may take some time as the random algorithm is not 
    that smart. *)
val dummy_ai: board -> team -> t * (int * int)

(** [smarter_random board piece] 
    Produces a random piece on the board and 10 valid coordinates for the 
    piece to move to, then picks the best one 
    according to the evaluation function. *)
val smarter_random: board -> team -> t * (int * int)

(** [minimax_ai board piece] 
    Produces a move on the current board, using the minimax tree to 
    a depth of 4. It chooses the best move according to 10000 possible moves
    that are generated at random in total. At each layer, 10 moves were 
    generated randomly. *)
val minimax_ai: board -> team -> t * (int * int)
