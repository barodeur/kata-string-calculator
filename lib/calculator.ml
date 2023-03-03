open Core

let int_of_string str = try Some (Int.of_string str) with _ -> None

module Delim = struct
  let re =
    Re2.create_exn
      ~options:{ Re2.Options.default with dot_nl = true }
      {|(//(;)\n)?(.*)$|}

  let split str =
    let m = Re2.first_match_exn re str in
    let delim = Re2.Match.get ~sub:(`Index 2) m |> Option.value ~default:"," in
    let numbers = Re2.Match.get_exn ~sub:(`Index 3) m in
    numbers |> String.split ~on:'\n'
    |> List.concat_map ~f:(String.split_on_chars ~on:(String.to_list delim))
    |> List.filter_map ~f:int_of_string
end

let sum str = str |> Delim.split |> List.fold ~init:0 ~f:( + )

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

let%expect_test "//;\n1;2" =
  printf "%d" (sum "//;\n1;2");
  [%expect {| 3 |}]
