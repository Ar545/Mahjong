type npc =
  | Npc1
  | Npc2
  | Npc3

type user = User

type player =
  | Npc of npc
  | User of user

let execute_round npc = ()
