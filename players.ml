type npc =
  | One
  | Two
  | Three

type advance =
  | One
  | Two
  | Three

type player =
  | Basic of npc
  | Adv of advance
  | User

type t = player list

let basic_players : t = [ User; Basic One; Basic Two; Basic Three ]

let adv_players : t = [ User; Adv One; Adv Two; Adv Three ]

let execute_round player = ()
