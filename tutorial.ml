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
  let bound = int_of_float i mod 2 in
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
        ];
        [ ah + a + 1; bh + b - 1; bh + b + 2 ];
        [ 600 + (11 * e); ch + c; 600 + (11 * d) ];
        [ ah + 9; ch + 1; 677 ];
      ]
  | _ -> []

let sne () =
  Unix.sleep 1;
  print_endline " "

let rec tutorial (chapter : int) : unit =
  if chapter = 0 then
    let chapter_0 () =
      ANSITerminal.print_string
        [ ANSITerminal.red; ANSITerminal.Bold ]
        "\n\nWelcome to the Mahjong Game Tutorial.\n";
      print_endline
        "This tutorial will cover the following information. To view a \
         desired chapter, enter the index of the chapter and follow by \
         enter.";
      sne ();
      ANSITerminal.print_string [ ANSITerminal.blue ] "1. Basics";
      sne ();
      ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Tiles";
      sne ();
      ANSITerminal.print_string [ ANSITerminal.green ] "3. Game";
      sne ();
      ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Scoring";
      sne ();
      ANSITerminal.print_string
        [ ANSITerminal.magenta; ANSITerminal.Underlined ]
        "5. Return to Main Menu\n";
      print_endline "Please select from 1 to 5:\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Please select from 1 to 5:";
          print_string "> "
      | anystring -> (
          match int_of_string_opt anystring with
          | None -> tutorial 0
          | Some int -> tutorial int)
    in
    chapter_0 ()
  else if chapter = 1 then
    let chapter_1 () =
      ANSITerminal.print_string [ ANSITerminal.blue ] "1. Basics\n";
      print_endline
        "Mahjong (Not Mahjong Solitaire!) is a tile-based game that \
         was developed in China around the 1600s.  ";
      Unix.sleep 3;
      print_endline
        "It is played with four players. Like the card game rummy, the \
         players sequencely draw and discard tiles, and compete to be \
         the  first one to met the winning goals.";
      Unix.sleep 2;
      print_endline
        "However, in our implementation of the game, you will play \
         against three AI players to achieve the same goals.";
      Unix.sleep 3;
      print_endline
        "There will be two modes: Easy, where you will play with \
         amature AIs; and Hard, where you will play with very smart \
         AIs.";
      Unix.sleep 2;
      print_endline
        "Now, you are good to go to the next Chapter. Press enter to \
         continue. Enter \"Quit\" to return to Main Menu";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> if anything = "Quit" then () else tutorial 2
    in
    chapter_1 ()
  else if chapter = 2 then
    let chapter_2 () =
      ANSITerminal.print_string [ ANSITerminal.cyan ] "2. Tiles\n";
      print_endline
        "The game is based on a set of 144 tiles (like cards) based on \
         Chinese characters and symbols";
      Unix.sleep 2;
      print_endline
        "144 tiles in this game are categorized into 5 groups:\n\
        \      Suits A - Bamboos (36 tiles)\n\
        \      Suits B - Dots (36 tiles)\n\
        \      Suits C - cCharacters (36 tiles)\n\
        \      Honors (28 tiles)\n\
        \      Bonuses (8 tiles)";
      Unix.sleep 3;
      print_endline
        "The three suits are very similar: each of them has numbers 1 \
         to 9, and each number has 4 duplicates, so that 4x9=36.\n\
        \          \
         🀇\t🀈\t🀉\t🀊\t🀋\t🀌\t🀍\t🀎\t🀏\t\n\
        \          \
         🀐\t🀑\t🀒\t🀓\t🀔\t🀕\t🀖\t🀗\t🀘\t\n\
        \          \
         🀙\t🀚\t🀛\t🀜\t🀝\t🀞\t🀟\t🀠\t🀡\t";
      Unix.sleep 3;
      print_endline
        "The honors are composed of 7 different types of tiles, namely \
         {East, South, West, North, Red, Green, White}, and each tile \
         has 4 duplicates.\n\
        \      🀀\t🀁\t🀂\t🀃\t🀄\t🀅\t🀆\t";
      Unix.sleep 3;
      print_endline
        "The bonuses are composed of 8 different tiles with no \
         duplicates, namely {Spring, Summer, Autumn, Winter, Plum, \
         Orchid, Bam, Chrysanthemum}.\n\
        \      🀢\t🀣\t🀤\t🀥\t🀦\t🀧\t🀨\t🀩";
      Unix.sleep 3;
      print_endline
        "All Tiles has identical backface, so that only you know your \
         tiles. Press enter to continue. Enter \"Quit\" to return to \
         Main Menu.";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> if anything = "Quit" then () else tutorial 3
    in
    chapter_2 ()
  else if chapter = 3 then
    let chapter_3 () =
      ANSITerminal.print_string [ ANSITerminal.green ] "3. Game\n";
      print_endline
        "Beginning of each game, all four players draw 13 tiles, while \
         the player on house draw an extra tile and discard one.";
      Unix.sleep 3;
      print_endline
        "Each player in turn, in counterclockwise direction, draws a \
         tile from the wall; the player proceeds to discard a tile \
         (either the tile just drawn, or a tile in the hand) to \
         maintain a hand of 13.";
      Unix.sleep 3;
      print_endline
        "A winning hand consists of 14 tiles. Players must win by \
         either drawing a piece from the wall that completes a 14-tile \
         hand (winning from the wall) or claiming a discard from \
         another player (winning by discard). The winning hand is \
         either made of four melds and one eyes (standard), or seven \
         eyes (all eyes), or all knitted tiles (explained later.) ";
      Unix.sleep 3;
      print_endline
        "Melds are groups of tiles within the player's hand. It may be \
         chow, pong, or kong. Chow is three suited tiles in sequence. \
         Pong is a set of three identical tiles. Kong is a set of four \
         identical tiles.";
      Unix.sleep 3;
      print_endline "example - chow 1";
      Unix.sleep 3;
      print_endline "example - chow 2";
      Unix.sleep 3;
      print_endline "example - pung";
      Unix.sleep 3;
      print_endline "example - kong";
      Unix.sleep 3;
      print_endline "example - eyes - HU";
      Unix.sleep 3;
      print_endline
        "Pro-tip: experienced players do not win every round. They \
         only HU a hand with good scores. Press enter to continue. \
         Enter \"Quit\" to return to Main Menu";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> if anything = "Quit" then () else tutorial 4
    in
    chapter_3 ()
  else if chapter = 4 then
    let chapter_4 () =
      ANSITerminal.print_string [ ANSITerminal.yellow ] "4. Scoring\n";
      print_endline
        "That's it. Press enter to end tutorial. Enter \"Replay\" to \
         replay from the beginning.";
      print_string "> ";
      match read_line () with
      | exception End_of_file ->
          print_endline "Press enter to continue.";
          print_string "> "
      | anything -> if anything = "Replay" then tutorial 0 else ()
    in
    chapter_4 ()
  else ()

let tutorial_start () = tutorial 0
