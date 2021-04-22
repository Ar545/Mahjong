open Players
open Tiles
open Command

type t = {
  house : player;
  house_seat : int;
  players : Players.t;
  mutable current_drawer : int;
  mutable tiles_count_left : int;
  hands : Tiles.t array;
  hands_open : Tiles.t array;
  mutable tiles_left : Tiles.t;
  mutable tiles_played : Tiles.t;
  mutable current_discard : Tiles.tile;
}

type round_end_message = {
  winner : player option;
  losers : player option;
  score : int;
}

let end_with_draw : round_end_message =
  { winner = None; losers = None; score = 0 }

exception End_of_tiles

exception Quit_game

exception Restart_round

exception Help_needed of t

exception Invalid of string

exception Winning of round_end_message

type result =
  | Quit_game
  | Round_end of round_end_message

let is_draw (res : result) : bool =
  match res with
  | Quit_game -> false
  | Round_end t -> (
      match t.winner with None -> true | Some t -> false)

let rec kong_draw_one state (konger_index : int) : unit =
  match state.tiles_left with
  | [] -> raise End_of_tiles
  | h :: t ->
      state.tiles_count_left <- state.tiles_count_left - 1;
      state.tiles_left <- t;
      if Tiles.is_bonus h then (
        (* redraw a tile when the draw is bonus *)
        state.hands_open.(konger_index) <-
          h :: state.hands_open.(konger_index);
        kong_draw_one state konger_index;
        ())
      else state.hands.(konger_index) <- h :: state.hands.(konger_index);
      ()

let rec draw_one state =
  match state.tiles_left with
  | [] -> raise End_of_tiles
  | h :: t ->
      state.tiles_count_left <- state.tiles_count_left - 1;
      state.tiles_left <- t;
      if Tiles.is_bonus h then (
        (* redraw a tile when the draw is bonus *)
        state.hands_open.(state.current_drawer) <-
          h :: state.hands_open.(state.current_drawer);
        draw_one state;
        ())
      else (
        state.hands.(state.current_drawer) <-
          h :: state.hands.(state.current_drawer);
        state.current_drawer <- (state.current_drawer + 1) mod 4;
        ())

let view_played (state : t) : unit =
  print_endline "Here are all the tiles played:";
  print_str_list (tiles_to_str state.tiles_played);
  Unix.sleep 2;
  print_endline "Here are player's open hand:";
  print_string "Ian:[";
  print_str_list (tiles_to_str state.hands_open.(1));
  print_string "]; Leo:[";
  print_str_list (tiles_to_str state.hands_open.(2));
  print_string "]; Andrew:[";
  print_str_list (tiles_to_str state.hands_open.(3));
  print_string "].";
  ()

let discard state (index : int) =
  let discard = List.nth state.hands.(0) index in
  state.hands.(0) <- List.filter (fun x -> x != discard) state.hands.(0);
  state.tiles_played <- discard :: state.tiles_played;
  state.current_discard <- discard;
  ()

let pung state = failwith "unimplemented"

let kong state = failwith "unimplemented"

let selfkong state = failwith "unimplemented"

let ankong state = failwith "unimplemented"

let chow state index_1 index_2 = failwith "unimplemented"

let win_round
    (state : t)
    (player : Players.player)
    (from_player : Players.player) : unit =
  failwith "unimplemented"

let take_command state command =
  let is_users_turn = state.current_drawer = 1 in
  match command with
  (* anytime, valid *)
  | Quit -> raise Quit_game
  | Restart -> raise Restart_round
  | Help -> raise (Help_needed state)
  | Played -> view_played state
  (* anytime, check *)
  | Mahjong ->
      let user = List.hd state.players in
      if is_users_turn then
        if winning_valid state.hands.(0) state.hands_open.(0) None then
          win_round state user (List.nth state.players 0)
        else
          raise (Invalid "your hand does not meet mahjong requirement")
      else if
        winning_valid state.hands.(0) state.hands_open.(0)
          (Some state.current_discard)
      then
        win_round state user
          (List.nth state.players (state.current_drawer - 1))
      else raise (Invalid "this discard is not valid to hu")
  | Kong ->
      if is_users_turn then
        if selfkong_valid state.hands_open.(0) state.hands.(0) then
          selfkong state
        else if ankong_valid_new state.hands.(0) then ankong state
        else raise (Invalid "this discard is not valid to kong")
      else if kong_valid state.hands.(0) state.current_discard then
        kong state
      else raise (Invalid "this discard is not valid to kong")
  (* player only, check *)
  | Discard int ->
      if is_users_turn then discard state int
      else raise (Invalid "not turn to discard")
  (* npc only, check *)
  | Continue ->
      if not is_users_turn then ()
      else raise (Invalid "must take action")
  | Pung ->
      if is_users_turn then
        raise (Invalid "you can only pung other's tiles")
      else if pung_valid state.hands.(0) state.current_discard then
        pung state
      else raise (Invalid "this discard is not valid to pung")
  | Chow (index_1, index_2) ->
      let is_upper_turn = state.current_drawer = 0 in
      if not is_upper_turn then
        raise (Invalid "you can only chow your upper hand's tiles")
      else if
        chow_index_valid state.hands.(0) index_1 index_2
          state.current_discard
      then chow state index_1 index_2
      else raise (Invalid "this discard is not valid to chow")

let init_round input_house input_players : t =
  let rec house_pos acc = function
    | h :: t -> if h = input_house then acc else house_pos (acc + 1) t
    | _ -> failwith "precondition violation"
  in
  let rec helper n state =
    match n with
    | 0 -> state
    | _ ->
        draw_one state;
        helper (n - 1) state
  in
  let house_seat_int = house_pos 0 input_players in
  helper 52
    {
      house = input_house;
      house_seat = house_seat_int;
      players = input_players;
      current_drawer = house_seat_int;
      tiles_count_left = tile_length (init_tiles ());
      hands = [| []; []; []; [] |];
      hands_open = [| []; []; []; [] |];
      tiles_left = init_tiles ();
      tiles_played = [];
      current_discard = Blank;
    }

let hand index t = tiles_to_str t.hands.(index)

let tiles_left t = tiles_to_str t.tiles_left

let discard_hint state =
  (* let kong_possible is implemented in tiles.ml *)
  (* let hu_possible is implemented in tiles.ml *)
  (* check hu *)
  if hu_possible state.hands.(0) then print_endline "you may hu now"
  else if (* check kong *)
          kong_possible state.hands.(0) then
    print_endline "you may kong now"
  else
    let discard_suggestion_tile = discard_suggestion state.hands.(0) in
    (* give discard suggestions *)
    print_string "you may discard now. we suggest,";
    print_endline (tile_string_converter discard_suggestion_tile)

let continue_hint state =
  (* let pung_possible is implemented in tiles.ml *)
  let continue_prompt = print_endline "no work to do. enter continue" in
  (* check pung *)
  if pung_possible state.hands.(0) state.current_discard then
    print_endline "you may pung now"
  else continue_prompt;
  (* check chow *)
  if state.current_drawer = 0 then
    (* let chow_possible is implemented in tiles.ml *)
    if chow_possible state.hands.(0) state.current_discard then
      print_endline "you may try chow now"
    else continue_prompt
  else continue_prompt

let resolve_help state =
  if state.current_drawer = 1 then
    (* current player is user *)
    discard_hint state
  else (* current player is npc *)
    continue_hint state

type move =
  | Legal
  | Illegal

let player_discard state : unit = failwith "unimplemented"

let npc_response state : unit = failwith "unimplemented"

let npc_discard state int : unit = failwith "unimplemented"

let player_response state int : unit = failwith "unimplemented"

let rec user_round state : unit =
  draw_one state;
  player_discard state;
  npc_response state;
  npc_int_round state 1

and npc_int_round state npc_int : unit =
  draw_one state;
  npc_discard state npc_int;
  player_response state npc_int;
  if npc_int = 3 then user_round state
  else npc_int_round state (npc_int + 1)

let rec start_rounds input_house input_players =
  let init_state = init_round input_house input_players in
  let start_rounds_loop state : result =
    let index = state.house_seat in
    match
      if index = 0 then user_round state else npc_int_round state index
    with
    | exception Quit_game -> Quit_game
    | exception Restart_round -> start_rounds state.house state.players
    | exception End_of_tiles -> Round_end end_with_draw
    | exception Winning message -> Round_end message
    | _ -> failwith "precondition vilation at start_round of roundstate"
  in
  start_rounds_loop init_state

(**********************************************)
(* some shit that ian left behind that leo do not understand *)
(**********************************************)

(* let hand = { abble: int; } *)

(* | {house;players;current;tiles_count_left;hands;tiles_left} as
   new_state -> match tiles_left with | [] -> failwith "No More Tiles" |
   h::t ->( hands.(current) <- h::hands.(current); {new_state with hands
   = hands}) *)

(* let next_turn t : t = failwith "TODO" *)
