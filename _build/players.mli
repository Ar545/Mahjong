(** Representation of the npc of the mahjong game *)

(** [t] is a player of the game*)
type player

type npc

type advance

type t = player array

val basic_npc : t

(** [execute_round npc] is the action the npc takes during its turn in a
    round *)
val execute_round : player -> unit
