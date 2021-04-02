type npc =
  | One
  | Two
  | Three

type player =
  | Npc of npc
  | User

let player_list = [ User; Npc One; Npc Two; Npc Three ]

let execute_round player = ()
