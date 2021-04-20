open Command
open GameState
open RoundState
open Tiles
open Tutorial

let new_round () = failwith "TODO"

let initalize () = failwith "TODO"

let print_str_list hand =
  ignore (List.map (fun x -> print_string (x ^ " ")) hand);
  ()

let tutorial () = Tutorial.tutorial_start ()

let quit_game () =
  print_endline "Game Over.";
  Stdlib.exit 0

let test () =
  print_string "\nHere are all the tiles\n";
  new_tiles |> tiles_to_str |> print_str_list;
  print_string "\n\nHere are the randomized tiles! \n";
  init_tiles () |> tiles_to_str |> print_str_list;
  print_string "\n\n";
  let game = init_game () in
  print_string "example player one hand:\n";
  current_round game |> hand 0 |> print_str_list;
  print_string "\n\nexample player two hand:\n";
  current_round game |> hand 1 |> print_str_list;
  print_string "\n\nexample player three hand:\n";
  current_round game |> hand 2 |> print_str_list;
  print_string "\n\nexample player four hand:\n";
  current_round game |> hand 3 |> print_str_list;
  print_string "\n\ntiles left after initial distribution";
  current_round game |> tiles_left |> print_str_list;
  print_string "\n\n\n";
  let game2 = init_game () in
  print_string "example player one hand:\n";
  current_round game2 |> hand 0 |> print_str_list;
  print_string "\n\nexample player two hand:\n";
  current_round game2 |> hand 1 |> print_str_list;
  print_string "\n\nexample player three hand:\n";
  current_round game2 |> hand 2 |> print_str_list;
  print_string "\n\nexample player four hand:\n";
  current_round game2 |> hand 3 |> print_str_list;
  print_string "\n\ntiles left after initial distribution";
  current_round game2 |> tiles_left |> print_str_list;
  print_string "\n\n\n"

let welcome_text () =
  ANSITerminal.print_string [ ANSITerminal.Bold ]
    "\n\nWelcome to Mahjong! \n";
  Unix.sleep 1;
  print_endline "Ian Presents!"

(* ; Unix.sleep 3 *)

let sleep_and_endline () =
  Unix.sleep 1;
  print_endline " "

let rec main_menu () =
  ANSITerminal.print_string
    [ ANSITerminal.red; ANSITerminal.Bold ]
    "MAIN MENU - Mahjong Game v.beta";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.blue ] "1. Play Easy\n";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Play Hard\n";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    "3. Display Tutorial\n";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Test\n";
  (* Unix.sleep 1; *)
  sleep_and_endline ();
  ANSITerminal.print_string
    [ ANSITerminal.magenta; ANSITerminal.Underlined ]
    "5. Quit Game\n";
  let rec match_input () =
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
            let not_ready () =
              print_string "Module Not Ready Yet. Quit to Main Menu.\n";
              main_menu ()
            in
            if integer = 1 then not_ready ()
            else if integer = 2 then not_ready ()
            else if integer = 3 then (
              tutorial ();
              print_endline
                "Tutorial Ends. Return to Main Menu in 5 seconds.";
              Unix.sleep 5;
              main_menu ())
            else if integer = 4 then test ()
            else if integer = 5 then quit_game ()
            else print_string "Invalid Index.";
            match_input ())
  in
  match_input ()

let main () =
  welcome_text ();
  main_menu ();
  print_string "End Test";
  quit_game ()

let () = main ()
