open Core

let int_of_string str = try Some (Int.of_string str) with _ -> None

let sum str =
  str |> String.split ~on:','
  |> List.concat_map ~f:(String.split ~on:'\n')
  |> List.filter_map ~f:int_of_string
  |> List.fold ~init:0 ~f:( + )

let%expect_test "empty string" =
  printf "%d" (sum "");
  [%expect {| 0 |}]

let%expect_test "3" =
  printf "%d" (sum "3");
  [%expect {| 3 |}]

let%expect_test "1,2" =
  printf "%d" (sum "1,2");
  [%expect {| 3 |}]

let%expect_test "1,2,3" =
  printf "%d" (sum "1,2,3");
  [%expect {| 6 |}]

let%expect_test "1\n2,3" =
  printf "%d" (sum "1\n2,3");
  [%expect {| 6 |}]
