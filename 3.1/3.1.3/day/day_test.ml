open Day
open OUnit2

let day_to_string d =
    match d with
    | Monday    -> "Monday"
    | Tuesday   -> "Tuesday"
    | Wednesday -> "Wednesday"
    | Thursday  -> "Thursday"
    | Friday    -> "Friday"
    | Saturday  -> "Saturday"
    | Sunday    -> "Sunday"

let make_day_test name exp_out input =
    name >:: (fun _ -> assert_equal exp_out (next_day input)
        ~printer:day_to_string)

let tests = "test suite for day" >::: [
    make_day_test "Mon" Tuesday Monday;
    make_day_test "Sat" Sunday Saturday;
    make_day_test "Sun" Monday Sunday;
]

let _ = run_test_tt_main tests
