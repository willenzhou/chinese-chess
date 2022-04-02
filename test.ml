open Commandtest
open Movetest
open OUnit2

let tests =
  "test suite for chess"  >::: List.flatten [
    Commandtest.tests;
    Movetest.tests

  ]

let _ = run_test_tt_main tests
