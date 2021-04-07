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

let inital_int_list () =
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
        600 + (11 * d);
        600 + (11 * d);
        600 + (11 * e);
        600 + (11 * e);
        600 + (11 * e);
      ]
  | _ -> []

let tutorial () =
  print_endline
    "At the beginning, the dealer, aka the player who play the \n\
     first hand (player on house) has 14 tiles and the player has 13 \
     cards."
