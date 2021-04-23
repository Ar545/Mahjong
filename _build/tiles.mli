(** Representation of one tile*)
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

(** Representation of the tiles left in a mahjong game round. Initialize
    with 144 tiles*)
type t = tile list

(**randomize the order if a list of tile*)
val shuffle : t -> t

(** Initialize with 144 tiles*)
val all_tiles : t

val init_tiles : unit -> t

val tile_length : t -> int

val add_tile_to_hand : tile -> t -> t

(** **Depreciated** [chow_valid hand t1 t2 t3] is true when t1 t2 t3 is
    a valid chow and t1 and t2 are both in hand. Otherwise, false.
    **Depreciated**)
val chow_valid_alternative : t -> tile -> tile -> tile -> bool

(** [chow_valid hand t1 t2 t3] is true when t1 t2 is valid position of
    hand and t1's tile and t2's tile and t3 is a valid chow. Otherwise,
    false*)
val chow_index_valid : t -> int -> int -> tile -> bool

(** [pung_valid hand tile] is true when [tile] can form a pung
    combination with [hand] and false otherwise*)
val pung_valid : t -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] and false otherwise*)
val kong_valid : t -> tile -> bool

(** **Depreciated** [kong_valid hand tile] is true when [tile] can form
    a kong combination with the tile [tile] and tile [tile] is within
    the tile list [hand] and false otherwise **Depreciated* *)
val ankong_valid : t -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] using the tile at index pos and false
    otherwise*)
val ankong_index_valid : t -> int -> bool

(** [winning_hand hand open_hand] is true when [hand] is a valid winning
    hand in mahjong and false otherwise*)
val winning_valid : t -> t -> tile option -> bool

(** [scoring hand open_hand] is the amount of points that is awarded to
    the player with the winning [hand]*)
val scoring : t -> t -> tile option -> int

val tiles_to_str : t -> string list

val tiles_to_index : t -> int list

(* val init_hand : t -> *)

val tile_index_converter : tile -> int

val index_to_tiles : int list -> t

val index_tile_converter : int -> tile

(** true if tile is a bonus *)
val is_bonus : tile -> bool

(** selfkong_valid open_hand hand is true if one can use one of the hand
    to kong some three in the open hand. AF: the hand is a valid hand.
    if hand does contain such combo then return true *)
val selfkong_valid : t -> t -> bool

(** ankong_valid_new hand is true if the hand has a valid hidden kong.
    automatically kong the first hidden kong find. AF: the hand is a
    valid hand. if hand does contain such pattern then return true *)
val ankong_valid_new : t -> bool

(** sort a list of tile by the standard order *)
val sort_hand : t -> t

(** sort a list of tile by the reverse of standard order *)
val rev_sort_hand : t -> t

(** separate the first tile from the list of tiles *)
val separate_first_tile : t -> t * tile

(** separate the last tile from the list of tiles *)
val separate_last_tile : t -> t * tile

(** separate a random tile from the list of tiles *)
val separate_random_tile : t -> t * tile
