open GameState
open Tutorial
open Tiles

(* [quit_game ()] ends the program *)
let quit_game () =
  print_endline "Game Over.";
  Stdlib.exit 0

(* [sleep_and_endline ()] halts (aka sleep) the program for 0.4 seconds
   and prints a separation border for new text to follow*)
let sleep_and_endline () =
  Unix.sleepf 0.3;
  print_endline "";
  print_endline "-------------------------------"

(* [welcome_text ()] prints the welcome texts when the program is
   launched *)
let welcome_text () =
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "\n\nWelcome to Mahjong! 🎮\n";
  sleep_and_endline ()

(* [main_menu ()] prints the menu of the mahjong game*)
let main_menu () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "MAIN MENU - Mahjong Game";
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "1. Play Easy Mode 📍";
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "2. Play Hard Mode ⚔";
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    "3. Display Tutorial 📋";
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "4. Game Settings ⚙";
  sleep_and_endline ();
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "5. Quit Game";
  sleep_and_endline ()

(* [not_ready ()] prints text informing user that the current feature
   has not been fully implemented. It then returns to main menu *)
let not_ready () =
  print_endline "===================================";
  print_endline
    "Module Not Ready Yet. Return to Main Menu. Enter 0 to test. 🕹 \n";
  Unix.sleep 1;
  main_menu ()

(* [play_game game] helps prompt the beginning of the mahjong game play *)
let rec play_game game =
  print_endline
    ("🎯 Begin Round " ^ (get_round game |> string_of_int) ^ ": ");
  print_endline
    ("🎰 Scores:\n" ^ string_of_scores game
   ^ "\n==========================\n");
  Unix.sleep 2;
  match update game with
  | Continue new_game -> play_game new_game
  | Quit new_game ->
      print_endline "Final Results: ";
      print_endline ("Scores:\n" ^ string_of_scores new_game ^ "\n")

(* [start_game advanced] initialized a mahjong with three basic npc or
   advanced npc depending on [advanced]. It prompts the beginning of the
   mahjong game play*)
let start_game play_advanced =
  let total_rounds = 8 in
  let game = init_game total_rounds play_advanced in
  let npc = game.players in
  print_string
    ("\nYou will be playing with "
    ^ Players.npc_list_to_string npc
    ^ " for a total of "
    ^ string_of_int total_rounds
    ^ " rounds! 🎲\n");
  sleep_and_endline ();
  play_game game;
  main_menu ()

(* [play_advanced ()] prints and prompt the user that the advanced level
   is selected *)
let play_advanced () =
  print_endline "===================================";
  Unix.sleepf 0.5;
  print_endline "You have selected Advanced Level!";
  Unix.sleep 1;
  start_game true

(* [play_basic ()] prints and prompt the user that the basic level is
   selected *)
let play_basic () =
  print_endline "===================================";
  Unix.sleepf 0.5;
  print_endline "You have selected Basic Level!";
  Unix.sleep 1;
  start_game false

(* [tutorial ()] begins the tutorial for the user *)
let tutorial () = Tutorial.tutorial_start ()

(** [Test] This module is for testing features. *)
module Test = struct
  (** [example_game ()] shows an example starting condition and play
      hands of a mahjong round*)
  let example_game () =
    let game = init_game 1 false in
    let round = RoundState.init_round game.house game.players in
    print_string "player one hand:\n";
    round |> RoundState.hand 0 |> print_str_list;
    print_string "\n\nplayer two hand:\n";
    round |> RoundState.hand 1 |> print_str_list;
    print_string "\n\nplayer three hand:\n";
    round |> RoundState.hand 2 |> print_str_list;
    print_string "\n\nplayer four hand:\n";
    round |> RoundState.hand 3 |> print_str_list;
    print_string "\n\ntiles left after initial distribution:";
    round |> RoundState.tiles_left |> print_str_list;
    print_string "\n\n\n"

  (** [test_start_game ()] tests and prints the result of the basic
      functionality of the mahjong game features*)
  let test_start_game () =
    print_string "\nHere are all the tiles\n";
    Tiles.all_tiles |> Tiles.tiles_to_str |> print_str_list;
    print_string "\n\nHere are the randomized tiles!\n\n\n";
    Tiles.init_tiles () |> Tiles.tiles_to_str |> print_str_list;
    print_string "\n\n";
    print_string "\nExample Round 1\n\n";
    example_game ();
    print_string "\nExample Round 2\n\n";
    example_game ()

  (** [test_adv ()] tests for advanced npc *)
  let test_adv () =
    example_game ();
    ()

  (** [test ()] tests according to instructive index *)
  let test inte = if inte = 0 then test_start_game () else test_adv ()
end

(** [test ()] make automatic playtests *)
let test int = Test.test int

let quit_game () =
  print_endline "Game Over. Thank You!\n";
  Stdlib.exit 0

let rec match_input () : unit =
  print_endline "Please select from 1 to 5: ";
  print_string "-> ";
  match read_line () with
  | exception End_of_file ->
      print_string "Invalid Input.";
      match_input ()
  | anystring -> (
      match int_of_string_opt anystring with
      | None ->
          print_string "Please use integer.";
          match_input ()
      | Some integer ->
          if integer = 1 then play_basic ()
          else if integer = 2 then play_advanced ()
          else if integer = 3 then (
            tutorial ();
            print_endline "Tutorial Ends. Return to Main Menu.";
            Unix.sleep 2;
            main_menu ())
          else if integer = 4 then not_ready ()
          else if integer = 5 then quit_game ()
          else if integer = 0 then test 0
          else if integer = -1 then test 1
          else if integer = -2 then test 2
          else print_string "Invalid Index.";
          match_input ())

(* [main ()] is the main program called when program initialized *)
let main () =
  welcome_text ();
  main_menu ();
  match_input ()

(* start program *)
let () = main ()
