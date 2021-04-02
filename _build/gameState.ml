open Players
open RoundState

type t = {
  round_num : int;
  termination_distance : int;
  scores : int array;
  players : player list;
  order : player list;
  game : RoundState.t;
}

let seating_order () = []

let termination_distance = 8

let init_game () : t =
  {
    round_num = 1;
    termination_distance;
    scores = [| 0; 0; 0; 0 |];
    players = player_list;
    order : player list;
    game : RoundState.t;
  }
