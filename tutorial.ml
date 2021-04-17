open Tiles

let random_abc_gen () =
  let i = Unix.gettimeofday () in
  let bound = int_of_float i mod 6 in
  if bound = 0 then [ 100; 200; 300 ]
  else if bound = 1 then [ 200; 300; 100 ]
  else if bound = 2 then [ 300; 100; 200 ]
  else if bound = 3 then [ 100; 300; 200 ]
  else if bound = 4 then [ 300; 200; 100 ]
  else [ 200; 100; 300 ]

let random_a () =
  let i = 10. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 5 in
  bound + 1

let random_b () =
  let i = 100. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 6 in
  bound + 2

let random_c () =
  let i = 1000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 7 in
  bound + 3

let random_d () =
  let i = 10000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 4 in
  bound + 1

let random_e () =
  let i = 100000. *. Unix.gettimeofday () in
  let bound = int_of_float i mod 3 in
  bound + 5

let initial_int_list () =
  match random_abc_gen () with
  | [ ah; bh; ch ] ->
      let a = random_a () in
      let b = random_b () in
      let c = random_c () in
      let d = random_d () in
      let e = random_e () in
      [
        ah + a;
        ah + a + 2;
        ah + 9;
        bh + b;
        bh + b + 1;
        ch + 1;
        ch + c;
        ch + c;
        600 + (11 * d);
        600 + (11 * d);
        600 + (11 * e);
        600 + (11 * e);
        600 + (11 * e);
      ]
  | _ -> []

let rec tutorial (chapter : int) : unit =
  if chapter = 0 then
    let chapter_0 () =
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n\nWelcome to the Mahjong Game Tutorial.\n";
      print_endline
        "\n\
         This tutorial will cover the following information. \n\n\
        \    To view sequencely, press enter. To skip to a desired \
         chapter, \n\n\
        \    enter the index of the chapter and follow by enter.\n\n";
      Unix.sleep 1;
      ANSITerminal.print_string [ ANSITerminal.blue ] "1. Basics\n";
      Unix.sleep 1;
      ANSITerminal.print_string [ ANSITerminal.green ] "2. Tiles\n";
      Unix.sleep 1;
      ANSITerminal.print_string [ ANSITerminal.cyan ] "3. Game\n";
      Unix.sleep 1;
      ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Scoring\n";
      print_endline "Please select from 1 to 4:\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Please select from 1 to 4:";
          print_string "> "
      | anystring -> (
          match int_of_string_opt anystring with
          | None -> tutorial 0
          | Some int -> tutorial int)
    in
    chapter_0 ()
  else if chapter = 1 then
    let chapter_1 () =
      print_endline
        "Mahjong (Not Mahjong Solitaire!) is a tile-based game that \
         was developed in China around the 1600s.  ";
      Unix.sleep 2;
      print_endline
        "It is played with four players. Like the card game UNO, the \
         four players sequencely make moves, and compete to be the  \
         first one to met the winning goals.";
      Unix.sleep 2;
      print_endline
        "However, in our implementation of the game, you will play \
         against three AI players to achieve the same goals.";
      Unix.sleep 2;
      print_endline
        "There will be two modes: Easy, where you will play with \
         amature AIs; and Hard, where you will play with very smart \
         AIs.";
      Unix.sleep 2;
      print_endline
        "Now, you are good to go to the next Chapter. Press enter to \
         continue.";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> tutorial 2
    in
    chapter_1 ()
  else if chapter = 2 then
    let chapter_2 () =
      print_endline
        "The game is based on a set of 144 tiles (like cards) based on \
         Chinese characters and symbols";
      Unix.sleep 2;
      ()
    in
    chapter_2 ()
  else if chapter = 3 then
    let chapter_3 () =
      print_endline
        "At the beginning, the dealer, aka the player who play the \n\
         first hand (player on house) has 14 tiles and the player has \
         13 cards.";
      Unix.sleep 2;
      ()
    in
    chapter_3 ()
  else if chapter = 4 then ()
  else ()

let tutorial_start () = tutorial 0
