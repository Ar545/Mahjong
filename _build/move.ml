open Main
open GameState
open Tutorial
open Tiles

(* This file is for testing features. *)

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

let test_graphics () =
  
  ()

let test_adv () = ()

let test inte =
  if inte = 0 then test_start_game ()
  else if inte = 1 then test_graphics ()
  else test_adv ()
