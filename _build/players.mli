(** Representation of the npc of the mahjong game *)

(** [t] is a player of the game*)
type npc =
  | One
  | Two
  | Three

type advance =
  | One
  | Two
  | Three

type player =
  | Basic of npc
  | Adv of advance
  | User

type t = player list

val player_to_string : player -> string

val npc_list_to_string : t -> string

val basic_players : t

val adv_players : t

(** [execute_round npc] is the action the npc takes during its turn in a
    round *)
val execute_round : player -> unit
