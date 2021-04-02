open Players

type order = player list

(* let hand = { abble: int; } *)

type t = {
  tiles_count_left : int;
  tiles_left : int list;
  next_player : player;
}

let init_round () : t = failwith "TODO"
