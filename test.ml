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

(* end tiles test *)

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

let suite =
  "Mahjong test suite"
  >::: List.flatten [ tiles_tests (* Insert Tests Here*) ]

let _ = run_test_tt_main suite
