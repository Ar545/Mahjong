open OUnit2
open Command
open RoundState
open GameState
open Players
open Tiles

(* open Tutorial *)

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

(* end tiles test *)

let tiles_list_1 =
  [ 104; 106; 109; 305; 306; 201; 203; 203; 633; 633; 677; 677; 677 ]
  |> index_to_tiles

let tiles_list_2 =
  [ 101; 103; 109; 207; 208; 301; 304; 304; 633; 633; 666; 666; 666 ]
  |> index_to_tiles

let tile_1a = index_tile_converter 105

let tile_1b = index_tile_converter 304

let tile_2a = index_tile_converter 102

let tile_2b = index_tile_converter 209

let tile_2c = index_tile_converter 206

let tiles_tests =
  [
    chow_valid_index_test "1a" tiles_list_1 1 2 tile_1a true;
    chow_valid_index_test "1b" tiles_list_1 4 5 tile_1b true;
    chow_valid_index_test "1n" tiles_list_1 1 2 tile_2b false;
    chow_valid_index_test "2a" tiles_list_2 1 2 tile_2a true;
    chow_valid_index_test "2b" tiles_list_2 4 5 tile_2b true;
    chow_valid_index_test "2c" tiles_list_2 4 5 tile_2c true;
    chow_valid_index_test "2n" tiles_list_2 4 5 tile_1a false;
  ]

let suite =
  "Mahjong test suite"
  >::: List.flatten [ tiles_tests (* Insert Tests Here*) ]

let _ = run_test_tt_main suite
