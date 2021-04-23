(** Representation of the dynamic game at a particular instance

    This module is the state of the game. It keeps track of the current
    house out of the four players, the points for each player, distance
    from termination, and the number of rounds that has elapsed. *)

(** [t] is the state of the entire games *)
type t = {
  round_num : int;
  termination_round : int;
  scores : int array;
  players : Players.player list;
  house : Players.player;
  house_streak : int;
  house_index : int;
  round : RoundState.t;
}

(** [game_progress] is a wrapper of the game states that indicates
    whether the game should continue or quit *)
type game_progress =
  | Quit of t
  | Continue of t

(** [current_round t] is the state of the current round of the game
    state [t] *)
val current_round : t -> RoundState.t

(** [init_game distance is_advanced] starts a new game state that
    represent the entirety of the mahjong game. [distance] is the amount
    of rounds that will be passed before the game terminates.
    [is_advanced] decides whether the user will play with three advanced
    level npc or three basic level npc*)
val init_game : int -> bool -> t

(** [update_round_results t results] takes in the current game state [t]
    and the results of the last round [results] and return whether the
    game will continue and initializa a new round or end the game *)
val update_round_results : t -> RoundState.result -> game_progress
