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

val shuffle : t -> t

(** Initialize with 144 tiles*)
val new_tiles : t

val init_tiles : unit -> t

val tile_length : t -> int

(** [chow_valid hand t1 t2 t3] is true when t1 t2 t3 is a valid chow and
    t1 and t2 are both in hand. Otherwise, false*)
val chow_valid : t -> tile -> tile -> tile -> bool

(** [pung_valid hand tile] is true when [tile] can form a pung
    combination with [hand] and false otherwise*)
val pung_valid : t -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] and false otherwise*)
val kong_valid : t -> tile -> bool

(** [winning_hand hand] is true when [hand] is a valid winning hand in
    mahjong and false otherwise*)
val winning_hand : t -> bool

(** [scoring hand] is the amount of points that is awarded to the player
    with the winning [hand]*)
val scoring : t -> int

val tiles_to_str : t -> string list

(* val init_hand : t -> *)
