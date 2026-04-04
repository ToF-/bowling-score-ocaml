open OUnit2
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

let bowling_list_gen =
  let open Gen in

  (* generate one frame (1 or 2 throws) *)
  let frame_gen =
    let* first = int_range 0 10 in
    if first = 10 then
      (* strike: only one throw in frame *)
      return [10]
    else
      let* second = int_range 0 (10 - first) in
      return [first; second]
  in

  (* build up to 10 frames, flatten, then truncate to 20 throws *)
  let rec frames n =
    if n = 0 then return []
    else
      let* f = frame_gen in
      let* rest = frames (n - 1) in
      return (f @ rest)
  in

  let* all_throws = frames 10 in
  return (List.take 20 all_throws)

  let gen = make bowling_list_gen
let score_is_less_or_equal_200 =
    Test.make  ~name: "score <= 200"
    gen (fun rolls -> Bowling.score rolls <= 200)

let _ =
    List.map run_test_tt_main [
            unit_tests ;
           ("tests" >::: List.map QCheck_ounit.to_ounit2_test [score_is_less_or_equal_200]) ;
    ]

