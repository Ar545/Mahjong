type npc =
  | One
  | Two
  | Three

type advance =
  | One
  | Two
  | Three

type player =
  | Npc of npc
  | Advance of advance
  | User

type t = player array

let basic_npc : t = [| User; Npc One; Npc Two; Npc Three |]

let execute_round player = ()
