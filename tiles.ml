type tile =
  | Dots of int
  | Bamboo of int
  | Characters of int
  | East
  | South
  | West
  | North
  | Red
  | Green
  | White
  | Plum
  | Orchid
  | Chrysanthemum
  | Bam
  | Spring
  | Summer
  | Autumn
  | Winter

type t = tile list

let dots_set =
  [
    Dots 1;
    Dots 2;
    Dots 3;
    Dots 4;
    Dots 5;
    Dots 6;
    Dots 7;
    Dots 8;
    Dots 9;
  ]

let bamboo_set =
  [
    Bamboo 1;
    Bamboo 2;
    Bamboo 3;
    Bamboo 4;
    Bamboo 5;
    Bamboo 6;
    Bamboo 7;
    Bamboo 8;
    Bamboo 9;
  ]

let characters_set =
  [
    Characters 1;
    Characters 2;
    Characters 3;
    Characters 4;
    Characters 5;
    Characters 6;
    Characters 7;
    Characters 8;
    Characters 9;
  ]

let orientations_set = [ East; South; West; North; Red; Green; White ]

let bonuses =
  [ Plum; Orchid; Chrysanthemum; Bam; Spring; Summer; Autumn; Winter ]

let all_tiles_variaty =
  dots_set @ bamboo_set @ characters_set @ orientations_set

(** A list of all the individual tiles with four of each. *)
let init_tiles =
  List.map (fun x -> [ x; x; x; x ]) all_tiles_variaty
  |> List.flatten |> ( @ ) bonuses

let chow_valid hand t1 t2 t3 = failwith "TODO"

let pung_valid hand tile = failwith "TODO"

let kong_valid hand tile = failwith "TODO"

let winning_hand hand tile = failwith "TODO"

let scoring hand = failwith "TODO"
