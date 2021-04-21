open Players
open RoundState

type t = {
  round_num : int;
  termination_distance : int;
  mutable scores : int array;
  players : player list;
  current_onhouse : int;
  mutable round : RoundState.t;
}

let current_round t = t.round

let seating_order () = []

let termination_distance = 8

let init_players is_adv = if is_adv then adv_players else basic_players

let init_game (distance : int) (is_adv : bool) : t =
  let init_players = init_players is_adv in
  let current = 0 in
  {
    round_num = 0;
    termination_distance = distance;
    scores = [| 0; 0; 0; 0 |];
    players = init_players;
    current_onhouse = current;
    round = init_round (List.nth init_players current) init_players;
  }
