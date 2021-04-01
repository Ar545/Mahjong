open Npc

type order = [Npc.t]

let hand = {

}

type t = {
  tiles_count_left : int;
  tiles_left : int list;
  next_player : npc.t;
}

let init_state () : t = 