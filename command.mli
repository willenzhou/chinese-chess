(**
    Representation of player command.
*)

(** The type [coord] represents a coordinate. *)
type coord = (int * int)

(** The type [command] represents a player command. *)
type command = 
  | Player
  | Ai of string
  | Learn
  | PieceName of string
  | Puzzle of string
  | Quit 
  | Move of (coord * coord)

(** Raised when a command is invalid. *)
exception InvalidCommand of string

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    start   "] is Start
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: InvalidCommand if [str] is the empty string, contains only spaces,
    or if the command is malformed. A command
    is invalid if the verb is neither "start", "quit", or "move",
    or if the verb is "start" or "quit" and there is a non-empty object phrase,
    or if the verb is "move" and there is an empty object phrase.*)
val parse: string -> command