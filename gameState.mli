(** Representation of the dynamic game at a particular instance

    This module is the state of the game. It keeps track of the current
    house out of the four players, the points for each player, distance
    from termination, and the number of rounds that has elapsed. *)

(** [t] is the state of the entire games *)
type t

val init_game : unit -> t
