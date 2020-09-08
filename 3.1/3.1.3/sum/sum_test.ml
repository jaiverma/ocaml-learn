open OUnit2
open Sum

let make_sum_test name expected_output input =
    name >:: (fun _ -> assert_equal expected_output (sum_fail input)
        ~printer:string_of_int)

let tests = "test suite for sum" >::: [
    "empty"  >:: (fun _ -> assert_equal 0 (sum []));
    "one"    >:: (fun _ -> assert_equal 1 (sum [1]));
    "onetwo" >:: (fun _ -> assert_equal 3 (sum [1;2]));
]

let tests_fail = "test suite for sum_fail" >::: [
    make_sum_test "empty2" 1 [];
    make_sum_test "empty" 0 [];
    make_sum_test "one" 1 [1];
    make_sum_test "onetwo" 3 [1; 2];
]

(* let _ = run_test_tt_main tests *)
let _ = run_test_tt_main tests_fail
