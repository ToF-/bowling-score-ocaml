open OUnit2

let suite =
    "bowling tests" >::: [
        "initially zero" >:: (fun _ -> assert_equal 0 (Bowling.score [])) ;
        "a single non strike throw is added" >:: (fun _ -> assert_equal 8 (Bowling.score [8]));
        "average throws are added" >:: (fun _ -> assert_equal 13 (Bowling.score [5; 2; 6])) ; 
        "a spare adds the next throw as bonus" >:: (fun _ -> assert_equal 14 (Bowling.score [3; 7; 2])) ;
        
    ]
let () =
  run_test_tt_main suite

