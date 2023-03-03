open Core

let capture_absolutely_everything f x = try Some (f x) with _ -> None

let sum str =
  str |> String.split ~on:','
  |> List.filter_map ~f:(capture_absolutely_everything Int.of_string)
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
