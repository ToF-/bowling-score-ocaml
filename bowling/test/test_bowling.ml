open OUnit
open QCheck

let unit_tests =
    "bowling tests" >::: [
        "initially zero" >:: 
            (fun _ -> assert_equal 0 (Bowling.score [])) ;
        "a single non strike throw is added" >:: 
            (fun _ -> assert_equal 8 (Bowling.score [8]));
        "average throws are added" >:: 
            (fun _ -> assert_equal 13 (Bowling.score [5; 2; 6])) ; 
        "a spare adds the next throw as bonus" >:: 
            (fun _ -> assert_equal 14 (Bowling.score [3; 7; 2])) ;
        "a strike add the next two throws as bonus" >:: 
            (fun _ -> assert_equal 24 (Bowling.score [10; 3; 3; 2])) ;
        "after 10 frames, special throws do not add bonus" >::
            (fun _ -> assert_equal ~printer:string_of_int 300 (Bowling.score [10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10])) ;
    ]

let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list nat_small)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.Test.make ~count:10
    ~name:"fail_sort_id"
    QCheck.(list nat_small)
    (fun l -> List.sort compare l = List.sort compare l);;

let _ =
    List.map run_test_tt_main [
            unit_tests ;
           ("tests" >::: List.map QCheck_ounit.to_ounit_test [passing; failing]) ;
    ]

