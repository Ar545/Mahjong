open Players

type t = {
  round : int;
  termination_distance : int;
  scores : int array;
  players : player list;
  order : Npc.t list;
}

let init_game () = failwith "TODO"

let seating_order () = failwith "TODO"
