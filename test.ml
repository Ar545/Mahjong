open OUnit2
open Command
open RoundState
open GameState
open Players

let suite =
  "Mahjong test suite" >::: List.flatten [ (* Insert Tests Here*) ]

let _ = run_test_tt_main suite
