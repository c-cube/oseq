module Q = QCheck
open OSeq

let spf = Printf.sprintf
let pp_ilist = Q.Print.(list int)
let plist f l = "[" ^ String.concat ";" (List.map f l) ^ "]"
let ppair f1 f2 (x, y) = Printf.sprintf "(%s,%s)" (f1 x) (f2 y)
let pint i = string_of_int i
let popt p x = Q.Print.option p x
let pilist l = plist pint l
let pistrlist l = plist (ppair pint (spf "%S")) l
let pilistlist l = plist (plist pint) l
let pi2list l = plist (ppair pint pint) l
let pstrlist l = plist (Printf.sprintf "%S") l
let lsort l = List.sort Stdlib.compare l
let ofll l = l |> of_list |> map of_list

let cmp_lii_unord l1 l2 : bool =
  List.sort Stdlib.compare l1 = List.sort Stdlib.compare l2

(* list of qcheck tests *)
let qchecks = ref []
let ounits = ref []

let add_qcheck line gen prop =
  let test = Q.Test.make gen prop ~name:(spf "qcheck %d" line) in
  qchecks := test :: !qchecks

let add_ounit f = ounits := f :: !ounits

(* compat test, ensure Seq.t and OSeq.t are the same *)
let () =
  add_ounit @@ fun () ->
  ignore (Seq.empty : int OSeq.t);
  ignore (OSeq.empty : int Seq.t)

let () =
  add_ounit @@ fun () ->
  let seq = empty in
  OUnit.assert_bool "empty" (is_empty seq);
  OUnit.assert_bool "empty"
    (try
       iter (fun _ -> raise Exit) seq;
       true
     with Exit -> false)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 0; 1; 2; 3; 4; 5 ] (0 -- 5 |> to_list);
  OUnit.assert_equal ~printer:pilist [ 0 ] (0 -- 0 |> to_list);
  OUnit.assert_equal ~printer:pilist [ 5; 4; 3; 2 ] (5 -- 2 |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 1; 2; 3; 4 ] (1 --^ 5 |> to_list);
  OUnit.assert_equal ~printer:pilist [ 5; 4; 3; 2 ] (5 --^ 1 |> to_list);
  OUnit.assert_equal ~printer:pilist [ 1 ] (1 --^ 2 |> to_list);
  OUnit.assert_equal ~printer:pilist [] (0 --^ 0 |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    (repeat 0 |> take 4 |> to_list)
    [ 0; 0; 0; 0 ];
  OUnit.assert_equal ~printer:pilist (repeat 1 |> take 0 |> to_list) []

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 0; 1; 2; 3; 4 ]
    (init 5 (fun i -> i) |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pi2list
    [ 0, 1; 1, 2; 2, 3 ]
    (mapi (fun i x -> i, x) (1 -- 3) |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 6; 12; 18; 24; 30 ]
    (filter_map
       (fun x ->
         if x mod 2 = 0 then
           Some (x * 3)
         else
           None)
       (1 -- 10)
    |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 0; 1; 2; 3; 4 ]
    (iterate (( + ) 1) 0 |> take 5 |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pistrlist
    [ 1, "b"; 0, "a" ]
    (foldi (fun i acc x -> (i, x) :: acc) [] (of_list [ "a"; "b" ]))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 1; 2; 1; 2; 1 ]
    (cycle (of_list [ 1; 2 ]) |> take 5 |> to_list);
  OUnit.assert_equal ~printer:pint 0
    (cycle (of_list [ 1; ~-1 ]) |> take 100_000 |> fold ( + ) 0)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (let f = function
       | 10 -> None
       | x -> Some (x, x + 1)
     in
     unfold f 0 |> to_list)

let () =
  add_qcheck __LINE__
    Q.(pair (fun1 Observable.int (small_list int)) (small_list int))
    (fun (f, l) ->
      of_list l
      |> flat_map (fun x -> of_list (Q.Fn.apply f x))
      |> to_list
      = CCList.flat_map (Q.Fn.apply f) l)

let () =
  add_qcheck __LINE__
    Q.(pair (fun1 Observable.int (small_list int)) (small_list int))
    (fun (f, l) ->
      of_list l
      |> flat_map (fun x -> of_list (Q.Fn.apply f x))
      |> to_list
      = (of_list l |> map (Q.Fn.apply f) |> map of_list |> flatten |> to_list))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pint 4 (nth 4 (0 -- 10));
  OUnit.assert_equal ~printer:pint 8 (nth 8 (0 -- 10))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_bool "t"
    (try
       ignore (nth 11 (1 -- 10));
       false
     with Not_found -> true)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pint ~-2
    (min ~lt:( < ) (of_list [ 1; 4; 6; 0; 11; -2 ]));
  OUnit.assert_bool "t"
    (try
       ignore (min ~lt:( < ) empty);
       false
     with Invalid_argument _ -> true)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pint 11
    (max ~lt:( < ) (of_list [ 1; 4; 6; 0; 11; -2 ]));
  OUnit.assert_bool "t"
    (try
       ignore (max ~lt:( < ) empty);
       false
     with Invalid_argument _ -> true)

let () =
  add_qcheck __LINE__
    (Q.pair (Q.list Q.small_int) (Q.list Q.small_int))
    (fun (l1, l2) -> equal Stdlib.( = ) (of_list l1) (of_list l2) = (l1 = l2))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(ppair pilist pilist)
    ([ 2; 4; 6; 8; 10 ], [ 1; 3; 5; 7; 9 ])
    ( partition (fun x -> x mod 2 = 0) (1 -- 10) |> fun (x, y) ->
      to_list x, to_list y )

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pi2list
    [ 0, 1; 1, 2; 2, 3; 3, 4; 4, 5 ]
    (zip_index (1 -- 5) |> to_list)

let () =
  add_qcheck __LINE__
    Q.(list (pair int int))
    (fun l ->
      let l = of_list l in
      let a, b = unzip l in
      equal ( = ) l (zip a b))

let () =
  add_qcheck __LINE__
    (Q.pair (Q.list Q.small_int) (Q.list Q.small_int))
    (fun (l1, l2) ->
      let sign x =
        if x < 0 then
          -1
        else if x = 0 then
          0
        else
          1
      in
      sign (compare Stdlib.compare (of_list l1) (of_list l2))
      = sign (Stdlib.compare l1 l2))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(popt pint)
    (find (fun x -> x >= 5) (1 -- 10))
    (Some 5);
  OUnit.assert_equal ~printer:(popt pint) (find (fun x -> x > 5) (1 -- 4)) None

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(popt pint)
    (find_map
       (fun x ->
         if x >= 5 then
           Some (-x)
         else
           None)
       (1 -- 10))
    (Some (-5));
  OUnit.assert_equal ~printer:(popt pint)
    (find_map
       (fun x ->
         if x > 5 then
           Some (-x)
         else
           None)
       (1 -- 4))
    None;
  OUnit.assert_equal ~printer:(popt pint)
    (find_map (fun _ -> None) (1 -- 10))
    None

let () =
  add_ounit @@ fun () -> OUnit.assert_equal ~printer:pint (sum (1 -- 10)) 55

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    (of_list [ 1; 2; 3; 4 ] |> take_while (fun x -> x < 4) |> to_list)
    [ 1; 2; 3 ]

let () =
  add_qcheck __LINE__
    (Q.pair (Q.list Q.small_int) Q.small_int)
    (fun (l, n) ->
      let s = of_list l in
      let s1, s2 = take n s, drop n s in
      append s1 s2 |> to_list = l)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pint 2
    (fold_while
       (fun acc b ->
         if b then
           acc + 1, `Continue
         else
           acc, `Stop)
       0
       (of_list [ true; true; false; true ]))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(plist pilist)
    [ []; [ 2 ]; [ 3; 2 ]; [ 4; 3; 2 ]; [ 5; 4; 3; 2 ]; [ 6; 5; 4; 3; 2 ] ]
    (scan (fun acc x -> (x + 1) :: acc) [] (1 -- 5) |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 0; 1; 3; 6; 10 ]
    (unfold_scan (fun acc x -> x + acc, acc) 0 (1 -- 5) |> to_list)

let () =
  add_qcheck __LINE__
    Q.(pair (small_list int) (small_list int))
    (fun (l1, l2) ->
      lsort (List.flatten @@ List.map (fun x -> List.map (fun y -> x, y) l2) l1)
      = lsort (product (of_list l1) (of_list l2) |> to_list))

let () =
  add_ounit @@ fun () ->
  let l =
    product (of_list [ true; false ]) (1 -- max_int) |> take 10 |> to_list
  in
  OUnit.assert_bool "a bit fair" (List.exists (fun (b, _) -> b = false) l)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(plist pilist) ~cmp:cmp_lii_unord
    [
      [ 1; 3; 4 ];
      [ 1; 3; 5 ];
      [ 1; 3; 6 ];
      [ 2; 3; 4 ];
      [ 2; 3; 5 ];
      [ 2; 3; 6 ];
    ]
    (to_list @@ cartesian_product @@ ofll [ [ 1; 2 ]; [ 3 ]; [ 4; 5; 6 ] ]);
  OUnit.assert_equal ~printer:(plist pilist) ~cmp:cmp_lii_unord []
    (to_list @@ cartesian_product @@ ofll [ [ 1; 2 ]; []; [ 4; 5; 6 ] ]);
  OUnit.assert_equal ~printer:(plist pilist) ~cmp:cmp_lii_unord [ [] ]
    (to_list @@ cartesian_product empty);
  OUnit.assert_equal ~printer:(plist pilist) ~cmp:cmp_lii_unord
    [ [ 1; 3; 4; 5; 6 ]; [ 2; 3; 4; 5; 6 ] ]
    (to_list @@ cartesian_product
    @@ ofll [ [ 1; 2 ]; [ 3 ]; [ 4 ]; [ 5 ]; [ 6 ] ])

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(plist pilist)
    [ [ 1; 1; 1 ]; [ 2; 2 ]; [ 3; 3 ]; [ 1 ] ]
    (of_list [ 1; 1; 1; 2; 2; 3; 3; 1 ] |> group ( = ) |> map to_list |> to_list)

let () =
  add_qcheck __LINE__
    Q.(small_list int)
    (fun l -> of_list l |> group ( = ) |> flatten |> to_list = l)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:(plist pilist)
    (List.map to_list [ 0 -- 24; 25 -- 49; 50 -- 74; 75 -- 99; 100 -- 100 ])
    (chunks 25 (0 -- 100) |> map Array.to_list |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [] (intersperse 0 empty |> to_list);
  OUnit.assert_equal ~printer:pilist [ 1 ] (intersperse 0 (return 1) |> to_list);
  OUnit.assert_equal ~printer:pilist
    [ 1; 0; 2; 0; 3; 0; 4; 0; 5 ]
    (intersperse 0 (1 -- 5) |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (merge
       (of_list
          [ of_list [ 1; 3; 5 ]; of_list [ 2; 4; 6 ]; of_list [ 7; 8; 9 ] ])
    |> to_list |> List.sort Stdlib.compare);
  OUnit.assert_equal ~printer:pilist [ 1; 2; 3; 4; 5; 6 ]
    (merge (of_list [ of_list [ 1; 3; 6 ]; of_list [ 2; 5 ]; of_list [ 4 ] ])
    |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_bool "t"
  @@ mem ( = ) (3, 5)
  @@ take 20_000 @@ merge
  @@ map (fun i -> iterate succ 0 |> map (fun j -> i, j))
  @@ iterate succ 0

let () =
  add_ounit @@ fun () ->
  let e = of_list [ 1 -- 3; 4 -- 6; 7 -- 9 ] in
  let e' = merge e in
  OUnit.assert_equal
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (to_list e' |> List.sort Stdlib.compare)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist [ 1; 2; 4; 8 ]
    (intersection Stdlib.compare
       (of_list [ 1; 1; 2; 3; 4; 8 ])
       (of_list [ 1; 2; 4; 5; 6; 7; 8; 9 ])
    |> to_list)

let () =
  add_qcheck __LINE__ (Q.list Q.small_int) (fun l ->
      zip_with (fun x y -> x, y) (of_list l) (of_list l)
      |> unzip |> fst |> to_list = l)

let () =
  add_ounit @@ fun () ->
  let e = zip_with ( + ) (repeat 1) (4 -- 7) in
  OUnit.assert_equal [ 5; 6; 7; 8 ] (to_list e)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    [ 1; 2; 2; 2; 3; 4; 5; 5; 6; 10; 11; 100 ]
    (sorted_merge Stdlib.compare
       (of_list [ 1; 2; 2; 3; 5; 10; 100 ])
       (of_list [ 2; 4; 5; 6; 11 ])
    |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilistlist
    [ [ 1; 4; 7; 10 ]; [ 2; 5; 8; 11 ]; [ 3; 6; 9; 12 ] ]
    (round_robin ~n:3 (1 -- 12) |> List.map to_list)

let () =
  add_ounit @@ fun () ->
  let e = round_robin ~n:2 (1 -- 10) in
  match e with
  | [ a; b ] ->
    OUnit.assert_equal ~printer:pilist [ 1; 3; 5; 7; 9 ] (to_list a);
    OUnit.assert_equal ~printer:pilist [ 2; 4; 6; 8; 10 ] (to_list b)
  | _ -> OUnit.assert_failure "wrong list lenght"

let () =
  add_ounit @@ fun () ->
  let e = round_robin ~n:3 (1 -- 999) in
  let l = List.map length e in
  OUnit.assert_equal ~printer:pilist [ 333; 333; 333 ] l

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilistlist
    [
      [ 1; 2; 3 ];
      [ 1; 3; 2 ];
      [ 2; 1; 3 ];
      [ 2; 3; 1 ];
      [ 3; 1; 2 ];
      [ 3; 2; 1 ];
    ]
    (permutations CCList.(1 -- 3) |> to_list |> List.sort Stdlib.compare);
  OUnit.assert_equal ~printer:pilistlist [ [] ] (permutations [] |> to_list);
  OUnit.assert_equal ~printer:pilistlist [ [ 1 ] ]
    (permutations [ 1 ] |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilistlist
    [ [ 1; 2 ]; [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ]; [ 3; 4 ] ]
    (combinations 2 (1 -- 4)
    |> map (List.sort Stdlib.compare)
    |> to_list |> List.sort Stdlib.compare);
  OUnit.assert_equal ~printer:pilistlist [ [] ]
    (combinations 0 (1 -- 4) |> to_list);
  OUnit.assert_equal ~printer:pilistlist [ [ 1 ] ]
    (combinations 1 (return 1) |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilistlist
    [ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ]; [ 1; 3 ]; [ 2 ]; [ 2; 3 ]; [ 3 ] ]
    (power_set (1 -- 3)
    |> map (List.sort Stdlib.compare)
    |> to_list |> List.sort Stdlib.compare);
  OUnit.assert_equal ~printer:pilistlist [ [] ] (power_set empty |> to_list);
  OUnit.assert_equal ~printer:pilistlist [ []; [ 1 ] ]
    (power_set (return 1)
    |> map (List.sort Stdlib.compare)
    |> to_list |> List.sort Stdlib.compare)

let () = add_qcheck __LINE__ Q.(array int) (fun a -> of_array a |> to_array = a)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pilist
    (of_array [| 1; 2; 3 |] |> to_list)
    [ 1; 2; 3 ];
  OUnit.assert_equal ~printer:(Q.Print.array pint)
    (of_list [ 1; 2; 3 ] |> to_array)
    [| 1; 2; 3 |]

let () =
  add_qcheck __LINE__
    Q.(pair (list string) string)
    (fun (s, sep) -> String.concat sep s = concat_string ~sep (of_list s))

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:Q.Print.string
    (concat_string ~sep:"" (of_list [ "a"; "b"; "coucou" ]))
    "abcoucou";
  OUnit.assert_equal ~printer:Q.Print.string
    (concat_string ~sep:"random" (return "a"))
    "a";
  OUnit.assert_equal ~printer:Q.Print.string
    (concat_string ~sep:"," (of_list [ "a"; "b"; "c"; ""; ""; "d" ]))
    "a,b,c,,,d";
  OUnit.assert_equal ~printer:Q.Print.string
    (concat_string ~sep:"random" empty)
    ""

let () =
  add_ounit @@ fun () ->
  let g =
    let n = ref 0 in
    fun () ->
      Some
        (incr n;
         !n)
  in
  let l = of_gen g in
  OUnit.assert_equal [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] (take 10 l |> to_list);
  OUnit.assert_equal [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] (take 10 l |> to_list);
  OUnit.assert_equal [ 11; 12 ] (drop 10 l |> take 2 |> to_list)

let () =
  add_ounit @@ fun () ->
  OUnit.assert_equal ~printer:pstrlist [ "abc"; "de"; "" ]
    (lines (of_string "abc\nde\n\n") |> to_list)

let () =
  add_qcheck __LINE__ Q.printable_string (fun s ->
      of_string s |> lines |> unlines |> to_string |> String.trim
      = String.trim s)

let () =
  add_ounit @@ fun () ->
  let naturals =
    Generator.(
      let rec aux n = yield n >>= fun () -> aux (n + 1) in
      run (aux 0))
  in
  let naturals' = unfold (fun n -> Some (n, n + 1)) 0 in
  OUnit.assert_equal
    ~printer:Q.Print.(list int)
    (take 100 naturals' |> to_list)
    (take 100 naturals |> to_list)

let () =
  add_qcheck __LINE__
    Q.(small_list int)
    (fun l ->
      let seq = of_list l in
      let seq2 =
        let open Generator in
        let rec aux seq =
          match seq () with
          | Nil -> empty
          | Cons (x, tl) -> yield x >>= fun () -> aux tl
        in
        run (aux seq)
      in
      equal Stdlib.( = ) seq seq2)

module IntK = struct
  type t = int

  let equal = ( = )
  let hash x = x land max_int
end

let () =
  add_ounit @@ fun () ->
  [ 1; 2; 3; 3; 2; 2; 3; 4 ] |> of_list
  |> group_by (module IntK) ~project:(fun x -> x)
  |> map snd |> sort Stdlib.compare |> to_list
  |> OUnit.assert_equal [ [ 1 ]; [ 2; 2; 2 ]; [ 3; 3; 3 ]; [ 4 ] ]

(* test for compat with seq *)
module Foo : module type of Seq = OSeq

let () =
  Printf.printf "running unit tests…\n%!";
  List.iter (fun t -> t ()) !ounits;
  Printf.printf "ran %d unit tests\n%!" (List.length !ounits);

  Printf.printf "running qcheck tests…\n%!";
  let errcode = QCheck_base_runner.run_tests ~colors:false !qchecks in
  if errcode <> 0 then exit errcode
