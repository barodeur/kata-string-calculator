open Core

exception NoNegative of int list

module Utils = struct
  let expr_re =
    Re2.create_exn
      ~options:{ Re2.Options.default with dot_nl = true }
      {|((?P<delimiters>//(\[(.*)\]+))\n)?(?P<numbers>.*)$|}

  let delimiter_re = Re2.create_exn {|\[([^]]+)\]|}
  let int_opt_of_string str = try Some (Int.of_string str) with _ -> None

  let sum str =
    let m = Re2.first_match_exn expr_re str in
    let delimiters =
      Re2.Match.get ~sub:(`Name "delimiters") m
      |> Option.value ~default:"[,]"
      |> Re2.get_matches_exn delimiter_re
      |> List.map ~f:(fun m -> Re2.Match.get_exn m ~sub:(`Index 1))
    in
    let numbers = Re2.Match.get_exn ~sub:(`Name "numbers") m in
    let positive, negative =
      List.fold ("\n" :: delimiters) ~init:[ numbers ]
        ~f:(fun numbs delimiter ->
          List.concat_map numbs
            ~f:(String.split_on_chars ~on:(String.to_list delimiter)))
      |> List.filter_map ~f:int_opt_of_string
      |> List.filter ~f:(( >= ) 1000)
      |> List.partition_tf ~f:(fun n -> n > 0)
    in
    if List.length negative > 0 then raise (NoNegative negative);
    positive |> List.fold ~init:0 ~f:( + )

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

  let%expect_test "//[;]\n1;2" =
    printf "%d" (sum "//[;]\n1;2");
    [%expect {| 3 |}]

  let%expect_test "1,-2" =
    (try sum "1,-2" |> ignore with NoNegative _ -> printf "raised" | _ -> ());
    [%expect {| raised |}]

  let%expect_test "1;-2;-3" =
    (try sum "1,-2,-3" |> ignore with
    | NoNegative l ->
        printf "raised %s" (l |> List.map ~f:Int.to_string |> String.concat)
    | _ -> ());
    [%expect {| raised -2-3 |}]

  let%expect_test "1000,2" =
    printf "%d" (sum "1000,2");
    [%expect {| 1002 |}]

  let%expect_test "1001,2" =
    printf "%d" (sum "1001,2");
    [%expect {| 2 |}]

  let%expect_test "//[***]\n1***2***3" =
    printf "%d" (sum "//[***]\n1***2***3");
    [%expect {| 6 |}]

  let%expect_test "//[*][%]\n1*2%3" =
    printf "%d" (sum "//[*][%]\n1*2%3");
    [%expect {| 6 |}]

  let%expect_test "//[**][%%]\n1**2%%3" =
    printf "%d" (sum "//[**][%%]\n1**2%%3");
    [%expect {| 6 |}]
end

class calculator =
  object
    val mutable sum_count = 0
    method get_sum_count = sum_count

    method sum str =
      sum_count <- sum_count + 1;
      Utils.sum str
  end

let%expect_test "" =
  let calc = new calculator in
  printf "%d" calc#get_sum_count;
  [%expect {| 0 |}];
  printf "%d" (calc#sum "1,2");
  [%expect {| 3 |}];
  printf "%d" calc#get_sum_count;
  [%expect {| 1 |}]
