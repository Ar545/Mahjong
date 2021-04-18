open Command
open GameState
open RoundState
open Tiles

let new_round () = failwith "TODO"

let initalize () = failwith "TODO"

let print_str_list hand =
  ignore (List.map (fun x -> print_string (x ^ " ")) hand);
  ()

let tutorial () = failwith "TODO"

let welcome_text () = failwith "TODO"

let main () =
  print_string "\n\nWelcome to Mahjong! \n";
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

let () = main ()
