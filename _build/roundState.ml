open Players
open Tiles

(* let hand = { abble: int; } *)

type t = {
  house : player;
  order : player list;
  tiles_count_left : int;
  tiles_left : Tiles.t;
}

let init_round house order : t =
  { house; order; tiles_count_left = 144; tiles_left = init_tiles }

let continue t : t = failwith "TODO"
