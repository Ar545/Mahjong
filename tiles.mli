(** Representation of one tile*)
type tile

(** Representation of the tiles left in a mahjong game round. Initialize
    with 144 tiles*)
type t = tile list

(** Initialize with 144 tiles*)
val init_tiles : unit -> tile list

(** [chow_valid hand t1 t2 t3] is true when t1 t2 t3 is a valid chow and
    t1 and t2 are both in hand. Otherwise, false*)
val chow_valid : tile list -> tile -> tile -> tile -> bool

(** [pung_valid hand tile] is true when [tile] can form a pung
    combination with [hand] and false otherwise*)
val pung_valid : tile list -> tile -> bool

(** [kong_valid hand tile] is true when [tile] can form a kong
    combination with [hand] and false otherwise*)
val kong_valid : tile list -> tile -> bool

(** [winning_hand hand] is true when [hand] is a valid winning hand in
    mahjong and false otherwise*)
val winning_hand : tile list -> bool

(** [scoring hand] is the amount of points that is awarded to the player
    with the winning [hand]*)
val scoring : tile list -> int
