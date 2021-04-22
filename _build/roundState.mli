(** Representation of a dynamic round within the game at a particular
    instance

    This module is the state of a round within the game. It keeps track
    of the number and identity of tiles left, the tile hand for each
    player that is hidden from other players, the tile hands that are
    shown, and the player of the current turn *)

(** [t] is the state of a round within the game*)
type t

(** initialize a state of t list of players while t is the house guy.
    return state *)
val init_round : Players.player -> Players.player list -> t

val hand : int -> t -> string list

val tiles_left : t -> string list

val take_command : t -> Command.command -> unit

(** start a round of t list of players while t is the house guy. raise
    @exception Quit_game when asked to quit game. raise @exception
    Winning of player and score when someone win the round. raise
    @exception end_of_tiles when the round draws. Never return unit. *)
val start_rounds : Players.player -> Players.t -> unit
