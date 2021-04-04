open Players
open Tiles

(* let hand = { abble: int; } *)

type t = {
  house : player;
  players : Players.t;
  mutable current : int;
  mutable tiles_count_left : int;
  hands : Tiles.t array;
  mutable tiles_left : Tiles.t;
}

let draw_one state =
  match state.tiles_left with
  | [] -> failwith "No More Tiles"
  | h :: t ->
      state.tiles_count_left <- state.tiles_count_left - 1;
      state.hands.(state.current) <- h :: state.hands.(state.current);
      state.tiles_left <- t;
      state.current <- (state.current + 1) mod 4

(* | {house;players;current;tiles_count_left;hands;tiles_left} as
   new_state -> match tiles_left with | [] -> failwith "No More Tiles" |
   h::t ->( hands.(current) <- h::hands.(current); {new_state with hands
   = hands}) *)

let play_one state tile = failwith "TODO"

let init_round house players : t =
  let rec helper n state =
    match n with
    | 0 -> state
    | _ ->
        draw_one state;
        helper (n - 1) state
  in
  helper 52
    {
      house;
      players;
      current = 0;
      tiles_count_left = tile_length (init_tiles ());
      hands = [| []; []; []; [] |];
      tiles_left = init_tiles ();
    }

let hand index t = tiles_to_str t.hands.(index)

let tiles_left t = tiles_to_str t.tiles_left

let next_turn t : t = failwith "TODO"
