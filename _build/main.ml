open Command
open GameState
open RoundState
open Tiles
open Tutorial
open Players

let quit_game () =
  print_endline "Game Over.";
  Stdlib.exit 0

let welcome_text () =
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "\n\nWelcome to Mahjong! \n";
  Unix.sleep 1;
  print_endline "Clarkson Presents!"

let print_str_list lst =
  ignore (List.map (fun x -> print_string (x ^ " ")) lst);
  ()

let sleep_and_endline () =
  Unix.sleep 1;
  print_endline "\n"

let main_menu () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "MAIN MENU - Mahjong Game v.beta";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.blue ] "1. Play Easy Mode";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Play Hard Mode";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.green ] "3. Display Tutorial";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Game Settings";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "5. Quit Game";
  sleep_and_endline ()

let not_ready () =
  print_endline "===================================";
  print_endline
    "Module Not Ready Yet. Return to Main Menu. Enter 0 to test.\n";
  Unix.sleep 1;
  main_menu ()

let rec play_game game =
  print_endline
    ("Begin Round "
    ^ (get_round game |> ( + ) 1 |> string_of_int)
    ^ ": ");
  print_endline
    ("Scores:\n" ^ string_of_scores game ^ "==================");
  match update game with
  | Continue new_game -> play_game new_game
  | Quit new_game ->
      print_endline "Final Results: ";
      print_endline ("Scores:\n" ^ string_of_scores new_game)

let start_game play_advanced =
  let total_rounds = 8 in
  let game = init_game total_rounds play_advanced in
  let npc = game.players in
  print_string
    ("\nYou will be playing with " ^ npc_list_to_string npc
   ^ " for a total of "
    ^ string_of_int total_rounds
    ^ " rounds!\n");
  sleep_and_endline ();
  play_game game;
  main_menu ()

let play_advanced () =
  print_endline "===================================";
  print_string "You have selected Advanced Level!\n";
  Unix.sleep 1;
  start_game true

let play_basic () =
  print_endline "===================================";
  print_string "You have selected Basic Level!\n";
  Unix.sleep 1;
  start_game false

(* let initalize () = failwith "TODO" *)

let tutorial () = Tutorial.tutorial_start ()

let quit_game () =
  print_endline "Game Over. Thank You!\n";
  Stdlib.exit 0

let example_game () =
  let game = init_game 1 false in
  let round = init_round game.house game.players in
  print_string "player one hand:\n";
  round |> hand 0 |> print_str_list;
  print_string "\n\nplayer two hand:\n";
  round |> hand 1 |> print_str_list;
  print_string "\n\nplayer three hand:\n";
  round |> hand 2 |> print_str_list;
  print_string "\n\nplayer four hand:\n";
  round |> hand 3 |> print_str_list;
  print_string "\n\ntiles left after initial distribution:";
  round |> tiles_left |> print_str_list;
  print_string "\n\n\n"

let test () =
  print_string "\nHere are all the tiles\n";
  all_tiles |> tiles_to_str |> print_str_list;
  print_string "\n\nHere are the randomized tiles!\n\n\n";
  init_tiles () |> tiles_to_str |> print_str_list;
  print_string "\n\n";
  print_string "\nExample Round 1\n\n";
  example_game ();
  print_string "\nExample Round 2\n\n";
  example_game ()

let rec match_input () : unit =
  print_endline "Please select from 1 to 5:";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      print_string "Invalid Input.";
      match_input ()
  | anystring -> (
      match int_of_string_opt anystring with
      | None ->
          print_string "Invalid Command.";
          match_input ()
      | Some integer ->
          if integer = 1 then play_basic ()
          else if integer = 2 then play_advanced ()
          else if integer = 3 then (
            tutorial ();
            print_endline
              "Tutorial Ends. Return to Main Menu in 5 seconds.";
            Unix.sleep 3;
            main_menu ())
          else if integer = 4 then not_ready ()
          else if integer = 5 then quit_game ()
          else if integer = 0 then test ()
          else print_string "Invalid Index.";
          match_input ())

let main () =
  welcome_text ();
  main_menu ();
  match_input ()

let () = main ()
