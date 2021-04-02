(** Representation of the npc of the mahjong game *)

(** [t] is a player of the game*)
type player

type user

type npc

val player_list : player list
(** [execute_round npc] is the action the npc takes during its turn in a
    round *)
val execute_round : player -> unit
