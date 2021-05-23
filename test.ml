open OUnit2
open Command
open RoundState
open GameState
open Players
open Tiles
open Tutorial

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

(* start tiles test *)

let chow_valid_index_test
    (name : string)
    (hand : Tiles.t)
    (t1 : int)
    (t2 : int)
    (t3 : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (chow_index_valid hand t1 t2 t3)

let chow_valid_depre_test
    (name : string)
    (hand : Tiles.t)
    (t1 : int)
    (t2 : int)
    (t3 : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (chow_index_valid hand t1 t2 t3)

let pung_valid_test
    (name : string)
    (hand : Tiles.t)
    (tile : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (pung_valid hand tile)

let kong_valid_test
    (name : string)
    (hand : Tiles.t)
    (tile : Tiles.tile)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (kong_valid hand tile)

let ankong_valid_index_test
    (name : string)
    (hand : Tiles.t)
    (tile : int)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (ankong_index_valid hand tile)

let hu_test
    (name : string)
    (hand : Tiles.t)
    (tile_option : tile option)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal ~printer:string_of_bool expected_output
    (winning_valid hand [] tile_option)

let discard_suggestion_test
    (name : string)
    (hand : Tiles.t)
    (expected_output : tile) : test =
  name >:: fun _ ->
  assert_equal expected_output (discard_suggestion hand)

(* end tiles test *)
let discard_tiles_1 =
  [
    101; 101; 101; 206; 207; 305; 305; 305; 633; 633; 633; 677; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_2 =
  [
    101; 101; 101; 205; 205; 207; 305; 306; 633; 633; 633; 677; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_3 =
  [
    101; 103; 105; 109; 201; 203; 205; 301; 303; 305; 307; 309; 655; 677;
  ]
  |> index_to_tiles

let discard_tiles_4 =
  [
    101; 103; 105; 109; 201; 203; 205; 301; 303; 305; 307; 309; 677; 677;
  ]
  |> index_to_tiles

let discard_tiles_5 =
  [
    101; 104; 106; 108; 108; 205; 301; 301; 302; 302; 303; 303; 666; 666;
  ]
  |> index_to_tiles

let discard_tiles_6 =
  [
    101; 104; 106; 108; 108; 205; 301; 301; 302; 302; 303; 303; 309; 666;
  ]
  |> index_to_tiles

let discard_tiles_7 =
  [
    101; 106; 108; 109; 205; 301; 301; 611; 622; 633; 644; 655; 655; 666;
  ]
  |> index_to_tiles

let discard_tiles_8 =
  [
    103; 108; 202; 205; 206; 207; 302; 303; 304; 305; 309; 611; 611; 677;
  ]
  |> index_to_tiles

let tiles_list_1 =
  [ 104; 106; 109; 305; 306; 201; 203; 203; 633; 633; 677; 677; 677 ]
  |> index_to_tiles

let tiles_list_2 =
  [ 101; 103; 109; 207; 208; 301; 304; 304; 633; 633; 666; 666; 666 ]
  |> index_to_tiles

let tile_1_chow_a = index_tile_converter 105

let tile_1_chow_b = index_tile_converter 304

let tile_1_pung_a = index_tile_converter 203

let tile_1_pung_b = index_tile_converter 633

let tile_2_pung_b = tile_1_pung_b

let tile_1_kong = index_tile_converter 677

let tile_2_chow_a = index_tile_converter 102

let tile_2_chow_b = index_tile_converter 209

let tile_2_chow_c = index_tile_converter 206

let tile_2_pung_a = index_tile_converter 304

let tile_false = index_tile_converter 611

let tile_2_kong = index_tile_converter 666

let hu_hand_1 =
  index_to_tiles
    [
      108;
      108;
      107;
      108;
      109;
      201;
      202;
      203;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_2 =
  index_to_tiles
    [
      101;
      102;
      103;
      104;
      105;
      106;
      107;
      108;
      109;
      101;
      102;
      103;
      666;
      666;
    ]

let hu_hand_3 =
  index_to_tiles
    [
      108;
      108;
      109;
      109;
      109;
      206;
      206;
      206;
      304;
      305;
      306;
      307;
      308;
      309;
    ]

let hu_hand_3_option =
  index_to_tiles
    [ 108; 108; 109; 109; 109; 206; 206; 304; 305; 306; 307; 308; 309 ]

let tiles_tests =
  [
    (* chow *)
    chow_valid_index_test "c1a" tiles_list_1 1 2 tile_1_chow_a true;
    chow_valid_index_test "c1b" tiles_list_1 4 5 tile_1_chow_b true;
    chow_valid_index_test "c1n" tiles_list_1 1 2 tile_2_chow_b false;
    chow_valid_index_test "c2a" tiles_list_2 1 2 tile_2_chow_a true;
    chow_valid_index_test "c2b" tiles_list_2 4 5 tile_2_chow_b true;
    chow_valid_index_test "c2c" tiles_list_2 4 5 tile_2_chow_c true;
    chow_valid_index_test "c2n1" tiles_list_2 4 5 tile_1_chow_a false;
    chow_valid_index_test "c2n2" tiles_list_2 4 5 tile_1_chow_b false;
    chow_valid_index_test "c2n3" tiles_list_2 4 5 tile_false false;
    (* chow_depre *)
    chow_valid_depre_test "c1a" tiles_list_1 1 2 tile_1_chow_a true;
    chow_valid_depre_test "c1b" tiles_list_1 4 5 tile_1_chow_b true;
    chow_valid_depre_test "c1n" tiles_list_1 1 2 tile_2_chow_b false;
    chow_valid_depre_test "c2a" tiles_list_2 1 2 tile_2_chow_a true;
    chow_valid_depre_test "c2b" tiles_list_2 4 5 tile_2_chow_b true;
    chow_valid_depre_test "c2c" tiles_list_2 4 5 tile_2_chow_c true;
    chow_valid_depre_test "c2n1" tiles_list_2 4 5 tile_1_chow_a false;
    chow_valid_depre_test "c2n2" tiles_list_2 4 5 tile_1_chow_b false;
    chow_valid_depre_test "c2n3" tiles_list_2 4 5 tile_false false;
    (* pung *)
    pung_valid_test "p1a" tiles_list_1 tile_1_pung_a true;
    pung_valid_test "p1b" tiles_list_1 tile_1_pung_b true;
    pung_valid_test "p1n1" tiles_list_1 tile_2_pung_a false;
    pung_valid_test "p1n2" tiles_list_1 tile_false false;
    pung_valid_test "p2a" tiles_list_2 tile_2_pung_a true;
    pung_valid_test "p2b" tiles_list_2 tile_2_pung_b true;
    pung_valid_test "p2n1" tiles_list_2 tile_1_pung_a false;
    pung_valid_test "p2n2" tiles_list_2 tile_false false;
    (* kong *)
    kong_valid_test "k1" tiles_list_1 tile_1_kong true;
    kong_valid_test "k1n1" tiles_list_1 tile_2_kong false;
    kong_valid_test "k1n2" tiles_list_1 tile_false false;
    kong_valid_test "k2" tiles_list_2 tile_2_kong true;
    kong_valid_test "k2n1" tiles_list_2 tile_1_kong false;
    kong_valid_test "k2n2" tiles_list_2 tile_false false;
    (*ankong*)
    ankong_valid_index_test "ak1n1" tiles_list_1 1 false;
    ankong_valid_index_test "ak1n2" tiles_list_1 11 false;
    ankong_valid_index_test "ak2n1" tiles_list_2 2 false;
    ankong_valid_index_test "ak2n2" tiles_list_2 12 false;
  ]

let random_tests () =
  (*random generator*)
  let random_list = initial_int_list () in
  match random_list with
  | [ the_list; chow; pung; others ] -> (
      match chow with
      | [ chowa; chowb; chowc ] -> (
          match pung with
          | [ kong; punga; pungb ] -> (
              match others with
              | [ oa; ob; oc ] ->
                  [
                    chow_valid_index_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowa)
                      true;
                    chow_valid_index_test "chow 4 5 random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 4 5 random b"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowb)
                      true;
                    chow_valid_index_test "chow 4 5 random c"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowc)
                      true;
                    chow_valid_index_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow 6 7 random a"
                      (index_to_tiles the_list)
                      6 7
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 7 8 random a"
                      (index_to_tiles the_list)
                      7 8
                      (index_tile_converter chowb)
                      false;
                    chow_valid_index_test "chow 8 9 random a"
                      (index_to_tiles the_list)
                      8 9
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow 2 3 random a"
                      (index_to_tiles the_list)
                      2 3
                      (index_tile_converter chowa)
                      false;
                    chow_valid_index_test "chow 3 4 random a"
                      (index_to_tiles the_list)
                      3 4
                      (index_tile_converter chowb)
                      false;
                    chow_valid_index_test "chow 9 10 random a"
                      (index_to_tiles the_list)
                      9 10
                      (index_tile_converter chowc)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oa)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter ob)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oc)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oa)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter ob)
                      false;
                    chow_valid_index_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oc)
                      false;
                    chow_valid_depre_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowa)
                      true;
                    chow_valid_depre_test "chow 4 5 random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 4 5 random b"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowb)
                      true;
                    chow_valid_depre_test "chow 4 5 random c"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter chowc)
                      true;
                    chow_valid_depre_test "chow 1 2 random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow 6 7 random a"
                      (index_to_tiles the_list)
                      6 7
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 7 8 random a"
                      (index_to_tiles the_list)
                      7 8
                      (index_tile_converter chowb)
                      false;
                    chow_valid_depre_test "chow 8 9 random a"
                      (index_to_tiles the_list)
                      8 9
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow 2 3 random a"
                      (index_to_tiles the_list)
                      2 3
                      (index_tile_converter chowa)
                      false;
                    chow_valid_depre_test "chow 3 4 random a"
                      (index_to_tiles the_list)
                      3 4
                      (index_tile_converter chowb)
                      false;
                    chow_valid_depre_test "chow 9 10 random a"
                      (index_to_tiles the_list)
                      9 10
                      (index_tile_converter chowc)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oa)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter ob)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      1 2
                      (index_tile_converter oc)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oa)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter ob)
                      false;
                    chow_valid_depre_test "chow false random a"
                      (index_to_tiles the_list)
                      4 5
                      (index_tile_converter oc)
                      false;
                    pung_valid_test "pung a random"
                      (index_to_tiles the_list)
                      (index_tile_converter punga)
                      true;
                    pung_valid_test "pung b random"
                      (index_to_tiles the_list)
                      (index_tile_converter pungb)
                      true;
                    pung_valid_test "pung false b random"
                      (index_to_tiles the_list)
                      (index_tile_converter ob)
                      false;
                    pung_valid_test "pung false a random"
                      (index_to_tiles the_list)
                      (index_tile_converter oa)
                      false;
                    kong_valid_test "kong random"
                      (index_to_tiles the_list)
                      (index_tile_converter kong)
                      true;
                    kong_valid_test "kong false a random"
                      (index_to_tiles the_list)
                      (index_tile_converter oa)
                      false;
                    kong_valid_test "kong false b random"
                      (index_to_tiles the_list)
                      (index_tile_converter ob)
                      false;
                    kong_valid_test "kong false c random"
                      (index_to_tiles the_list)
                      (index_tile_converter oc)
                      false;
                  ]
              | _ -> failwith "precondition violation")
          | _ -> failwith "precondition violation")
      | _ -> failwith "precondition violation")
  | _ -> failwith "precondition violation"

let possibility_test = []

let win_test =
  [
    hu_test "hu_test_1" hu_hand_1 None true;
    hu_test "hu_test_2" hu_hand_2 None true;
    hu_test "hu_test_3" hu_hand_3 None true;
    hu_test "hu_test_3_option" hu_hand_3_option
      (Some (index_tile_converter 206))
      true;
  ]

let discard_suggestion_tests =
  [
    discard_suggestion_test "discard test 1" hu_hand_2
      (index_tile_converter 666);
    discard_suggestion_test "discard test 2" discard_tiles_1
      (index_tile_converter 207);
    discard_suggestion_test "discard test 3" discard_tiles_2
      (index_tile_converter 207);
    discard_suggestion_test "discard test 4" discard_tiles_3
      (index_tile_converter 655);
    discard_suggestion_test "discard test 5" discard_tiles_4
      (index_tile_converter 309);
    discard_suggestion_test "discard test 6" discard_tiles_5
      (index_tile_converter 101);
    discard_suggestion_test "discard test 7" discard_tiles_6
      (index_tile_converter 666);
    discard_suggestion_test "discard test 8" discard_tiles_7
      (index_tile_converter 611);
    discard_suggestion_test "discard test 9" discard_tiles_8
      (index_tile_converter 677);
  ]

let suite =
  "Mahjong test suite"
  >::: List.flatten
         [
           random_tests ();
           possibility_test;
           random_tests ();
           tiles_tests;
           random_tests ();
           win_test;
           random_tests ();
           discard_suggestion_tests;
         ]

let _ = run_test_tt_main suite
