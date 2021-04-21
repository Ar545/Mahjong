(** Representation of a dynamic round within the game at a particular
    instance

    This module is the state of a round within the game. It keeps track
    of the number and identity of tiles left, the tile hand for each
    player that is hidden from other players, the tile hands that are
    shown, and the player of the current turn *)

(** [t] is the state of a round within the game*)

type t

val init_round : Players.player -> Players.player list -> t

val hand : int -> t -> string list

val tiles_left : t -> string list

val take_command : t -> Command.command -> unit
