open Command
open Tiles

let new_round () = failwith "TODO"

let initalize () = failwith "TODO"

let print_tiles hand =
  ignore (List.map (fun x -> print_string (x ^ " ")) hand);
  ()

let main () =
  print_string "\nWelcome to Mahjong! \n";
  print_string "\nHere are all the tiles in a round! \n";
  tiles_to_str init_tiles |> print_tiles

let () = main ()
