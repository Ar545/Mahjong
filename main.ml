open Command
open Tiles

let new_round () = failwith "TODO"

let initalize () = failwith "TODO"

let print_tiles hand =
  let rec helper = function
    | [ h ] ->
        print_string h;
        ()
    | h :: t ->
        print_string h;
        helper t
    | _ -> failwith "Print tiles failed"
  in
  helper hand

let main () = print_tiles Tiles.init_tiles

let () = main ()
