(**************************************************************************
  tile def start*)

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

let tile_index_converter = function
  | Dots (num : int) -> 200 + num
  | Bamboo (num : int) -> 100 + num
  | Characters (num : int) -> 300 + num
  | Blank -> 0
  | East -> 611
  | South -> 622
  | West -> 633
  | North -> 644
  | Red -> 655
  | Green -> 666
  | White -> 677
  | Plum -> 811
  | Orchid -> 822
  | Chrysanthemum -> 833
  | Bam -> 844
  | Spring -> 855
  | Summer -> 866
  | Autumn -> 877
  | Winter -> 888

let tiles_to_index hand = List.map tile_index_converter hand

(**************************************************************************
  tile define - end - init hand - start*)

(** time () returns a random int from the system time. limit the int to
    at most 10000, at few 0 by the mod in the end *)
let time () = (Unix.time () |> int_of_float) mod 10000

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
let all_tiles : t =
  List.map (fun x -> [ x; x; x; x ]) all_tiles_variety
  |> List.flatten |> ( @ ) bonuses

let init_tiles () : t = shuffle all_tiles

(***********************************************************************
  init hand - end - c,p,kong - start*)

let get_index_of_tile = function
  | Dots int -> int
  | Bamboo int -> int
  | Characters int -> int
  | _ -> 0

let get_shape_of_tile = function
  | Dots int -> 1
  | Bamboo int -> 2
  | Characters int -> 3
  | _ -> 0

(** [chow_valid hand t1 t2 t3] is true if t1 t2 t3 is a chow. \n AF: the
    (hand : tile list) must already contain t1 and t2, otherwise it is
    not a valid chow. In other words, if a person chow 1 2 to 3 without
    having 1 2, system will retrun 'invalid command, no such 1 and 2'.
    else, here to determine if 1 2 3 forms a valid chow, and if not
    return 'invalid chow, 1 2 3 is not a chow'*)
let chow_valid (hand : t) t1 t2 t3 =
  let list_shape = List.map get_shape_of_tile [ t1; t2; t3 ] in
  match list_shape with
  | [ a; b; c ] ->
      if a == b && b == c then
        let sorted_index =
          List.map get_index_of_tile [ t1; t2; t3 ] |> List.sort compare
        in
        match sorted_index with
        | [ a; b; c ] ->
            if a + 1 == b && b + 1 == c then true else false
        | _ -> false
      else false
  | _ -> false

let chow_valid_alternative (hand : t) t1 t2 (t3 : tile) =
  let (index_list : int list) = tiles_to_index [ t1; t2; t3 ] in
  match index_list with
  | [ a; b; c ] -> if a + 1 == b && b + 1 == c then true else false
  | _ -> false

(**raise invalid index exception if the player input incorrect index
   when chow*)
exception Invalid_index

(**if exception unknown is raise, then there is bug in the code *)
exception Unknown

(**chow_index_valid (hand : t) (index1 : int) (index2 : int) t3 take one
   tile t3 to chow and two index of the hand t. Assert index 1 and 2 are
   valid position of the tile, meaning, for a standard hand, 1 to 13. *)
let chow_index_valid (hand : t) (index1 : int) (index2 : int) t3 =
  let hand_length = List.length hand in
  if
    index1 < 1 || index2 < 1 || index1 > hand_length
    || index2 > hand_length
  then raise Invalid_index
  else
    chow_valid hand
      (List.nth hand (index1 - 1))
      (List.nth hand (index2 - 1))
      t3

(**count_tile (hand : t) tile (0 : int) return the number of tile in a
   hand *)
let rec count_tile (hand : t) tile (acc : int) =
  match hand with
  | h :: t ->
      if h = tile then count_tile t tile (acc + 1)
      else count_tile t tile acc
  | [] -> acc

(** [pung_valid hand tile] is true if t is a pung. AF: the hand is a
    valid hand. if hand does contain two tile_of_question then return
    true*)
let pung_valid hand tile =
  if count_tile hand tile 0 > 1 then true else false

let kong_valid hand tile =
  if count_tile hand tile 0 > 2 then true else false

let ankong_valid hand tile =
  if count_tile hand tile 0 > 3 then true else false

let ankong_index_valid hand (pos : int) =
  let tile = List.nth hand pos in
  if count_tile hand tile 0 > 3 then true else false

(**************************************************************************
  cpkong - end - winning and scoring - start*)

let compare (t1 : tile) (t2 : tile) =
  tile_index_converter t1 - tile_index_converter t2

let rec add_tile_to_hand tile = function
  | tile' :: t as hand ->
      if compare tile tile' <= 0 then tile :: hand
      else tile' :: add_tile_to_hand tile t
  | [] -> [ tile ]

let check_size_13 hand = if 13 == List.length hand then true else false

let rec find_trio = function
  | t1 :: t2 :: t3 :: tail ->
      if (t1 = t2 && t2 = t3) || (t1 + 1 = t2 && t2 + 1 = t3) then
        find_trio tail
      else false
  | [] -> true
  | _ -> false

let rec find_trump checked_hand = function
  | t1 :: t2 :: tail ->
      if t1 = t2 then find_trio (checked_hand @ tail)
      else find_trump (t1 :: t2 :: checked_hand) tail
  | t1 :: tail -> false
  | [] -> false

let winning_hand_standard hand open_hand =
  find_trump [] (tiles_to_index hand)

let rec check_seven = function
  | t1 :: t2 :: tail -> if t1 = t2 then check_seven tail else false
  | [] -> true
  | _ -> raise Unknown

(**the compare function to compare tiles is not yet implemented*)
let winning_hand_seven hand =
  if check_size_13 hand then
    let sorted_together = List.sort compare hand in
    check_seven sorted_together
  else false

let rec check_thirteen = function
  | t1 :: t2 :: tail ->
      if 2 < t1 - t2 then check_thirteen (t2 :: tail) else false
  | t1 :: tail -> true
  | _ -> raise Unknown

let winning_hand_thirteen (hand : t) =
  if check_size_13 hand then
    let sorted_together = List.sort compare hand in
    check_thirteen (tiles_to_index sorted_together)
  else false

let winning_hand (hand : t) (open_hand : t) (current : tile option) =
  let complete_hand =
    match current with None -> hand | Some tile -> tile :: hand
  in
  if winning_hand_standard complete_hand open_hand then 1
  else if winning_hand_seven complete_hand then 2
  else if winning_hand_thirteen complete_hand then 4
  else 0

let winning_valid (hand : t) (open_hand : t) (current : tile option) =
  if winning_hand hand open_hand current <> 0 then true else false

let scoring hand open_hand (current : tile option) =
  let (score : int ref) = ref (winning_hand hand open_hand current) in
  let () = match current with None -> score := !score * 2 | _ -> () in
  let () = match open_hand with [] -> score := !score * 2 | _ -> () in
  !score

(***************************************************************************
  winning and scoring - end - tile-printer - start*)

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

(**************************************************************************
  tile-printer - end - wildcard win - start*)

let wildcard_score hand open_hand (current : tile option) =
  scoring hand open_hand current / 2

let wildcard_winning_valid
    (wildcard : tile)
    (subsitute_list : t)
    hand
    open_hand
    (current : tile option) =
  let clean_list =
    let not_wild m = wildcard <> m in
    List.filter not_wild hand
  in
  winning_valid (subsitute_list @ clean_list) open_hand current

(**************************************************************************
  end - wildcard win *)

let index_tile_converter (i : int) =
  if i = 0 then Blank
  else if i < 110 then Bamboo (i - 100)
  else if i < 210 then Dots (i - 200)
  else if i < 310 then Characters (i - 300)
  else if i = 611 then East
  else if i = 622 then South
  else if i = 633 then West
  else if i = 644 then North
  else if i = 655 then Red
  else if i = 666 then Green
  else if i = 677 then White
  else if i = 811 then Plum
  else if i = 822 then Orchid
  else if i = 833 then Chrysanthemum
  else if i = 844 then Bam
  else if i = 855 then Spring
  else if i = 866 then Summer
  else if i = 877 then Autumn
  else if i = 888 then Winter
  else raise Unknown

let index_to_tiles hand = List.map index_tile_converter hand

(** true if tile is a bonus *)
let is_bonus = function
  | Plum | Orchid | Chrysanthemum | Bam | Spring | Summer | Autumn
  | Winter ->
      true
  | _ -> false

(** selfkong_valid open_hand hand is true if one can use one of the hand
    to kong some three in the open hand. AF: the hand is a valid hand.
    if hand does contain such combo then return true *)
let rec selfkong_valid open_hand hand =
  match hand with
  | h :: t ->
      if kong_valid open_hand h then true
      else selfkong_valid open_hand t
  | [] -> false

let ankong_tile_opt hand = List.find_opt (ankong_valid hand) hand

(** ankong_valid_new hand is true if the hand has a valid hidden kong.
    automatically kong the first hidden kong find. AF: the hand is a
    valid hand. if hand does contain such pattern then return true *)
let ankong_valid_new hand =
  match ankong_tile_opt hand with None -> false | Some t -> true

let sort_hand hand =
  index_to_tiles (List.sort Stdlib.compare (tiles_to_index hand))

let rev_sort_hand hand = List.rev (sort_hand hand)

let separate_first_tile hand =
  match sort_hand hand with
  | h :: t -> (t, h)
  | [] -> failwith "precondition violation"

let separate_random_tile hand =
  match shuffle hand with
  | h :: t -> (sort_hand t, h)
  | [] -> failwith "precondition violation"

let separate_last_tile hand =
  let rev = rev_sort_hand hand in
  match rev with
  | h :: t -> (sort_hand t, h)
  | [] -> failwith "precondition violation"
