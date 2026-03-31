open OUnit2

let score_with_zero_throws_is_zero _ =
  assert_equal 0 (Bowling.score 0)

let suite =
    "bowling tests" >::: [
        "initially zero" >:: score_with_zero_throws_is_zero;
    ]
let () =
  run_test_tt_main suite

