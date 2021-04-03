open Players
open RoundState

type t = {
  round_num : int;
  termination_distance : int;
  mutable scores : int array;
  players : player array;
  current : int;
  mutable round : RoundState.t;
}

let current_round t = t.round

type move =
  | Legal
  | Illegal

let seating_order () = []

let termination_distance = 8

let players = basic_npc

let current = 0

let init_game : t =
  {
    round_num = 0;
    termination_distance;
    scores = [| 0; 0; 0; 0 |];
    players;
    current;
    round = init_round players.(current) players;
  }
