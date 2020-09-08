open OUnit2
open Sum

let tests = "test suite for sum" >::: [
    "empty"  >:: (fun _ -> assert_equal 0 (sum []));
    "one"    >:: (fun _ -> assert_equal 1 (sum [1]));
    "onetwo" >:: (fun _ -> assert_equal 3 (sum [1;2]));
]

let tests_fail = "test suite for sum_fail" >::: [
    "empty2" >:: (fun _ -> assert_equal 1 (sum_fail []));
    "empty"  >:: (fun _ -> assert_equal 0 (sum_fail []));
    "one"    >:: (fun _ -> assert_equal 1 (sum_fail [1]));
    "onetwo" >:: (fun _ -> assert_equal 3 (sum_fail [1;2]));
]

(* let _ = run_test_tt_main tests *)
let _ = run_test_tt_main tests_fail
