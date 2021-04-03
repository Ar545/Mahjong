type tile =
  | Blank
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

let all_tiles_variety =
  dots_set @ bamboo_set @ characters_set @ orientations_set

let tile_length tiles = List.length tiles

(** time () returns a random int from the system time. limit the int to
    at most 1000000, at few 0 by the mod in the end *)
let time () = (Unix.time () |> int_of_float) mod 1000000

let shuffle t =
  let compare x y =
    match x with a, b -> ( match y with c, d -> a - c)
  in
  let random_arr =
    List.map (fun a -> (Random.bits () mod time (), a)) t
  in
  let sorted = List.sort compare random_arr in
  List.map snd sorted

(** A list of all the individual tiles with four of each. *)
let new_tiles : t =
  List.map (fun x -> [ x; x; x; x ]) all_tiles_variety
  |> List.flatten |> ( @ ) bonuses

let init_tiles () : t = shuffle new_tiles

exception Unknown

let chow_valid hand t1 t2 t3 = failwith "TODO"

let pung_valid hand tile = failwith "TODO"

let kong_valid hand tile = failwith "TODO"

let winning_hand hand = failwith "TODO"

let scoring hand = failwith "TODO"

let tile_string_converter = function
  | Dots int -> (
      match int with
      | 1 -> "🀙"
      | 2 -> "🀚"
      | 3 -> "🀛"
      | 4 -> "🀜"
      | 5 -> "🀝"
      | 6 -> "🀞"
      | 7 -> "🀟"
      | 8 -> "🀠"
      | 9 -> "🀡"
      | _ -> raise Unknown)
  | Bamboo int -> (
      match int with
      | 1 -> "🀐"
      | 2 -> "🀑"
      | 3 -> "🀒"
      | 4 -> "🀓"
      | 5 -> "🀔"
      | 6 -> "🀕"
      | 7 -> "🀖"
      | 8 -> "🀗"
      | 9 -> "🀘"
      | _ -> raise Unknown)
  | Characters int -> (
      match int with
      | 1 -> "🀇"
      | 2 -> "🀈"
      | 3 -> "🀉"
      | 4 -> "🀊"
      | 5 -> "🀋"
      | 6 -> "🀌"
      | 7 -> "🀍"
      | 8 -> "🀎"
      | 9 -> "🀏"
      | _ -> raise Unknown)
  | Blank -> " 🀫 "
  | East -> "🀀"
  | South -> "🀁"
  | West -> "🀂"
  | North -> "🀃"
  | Red -> "🀄"
  | Green -> "🀅"
  | White -> "🀆"
  | Plum -> "🀢"
  | Orchid -> "🀣"
  | Chrysanthemum -> "🀥"
  | Bam -> "🀤"
  | Spring -> "🀦"
  | Summer -> "🀧"
  | Autumn -> "🀨"
  | Winter -> "🀩"

let tiles_to_str hand = List.map tile_string_converter hand
