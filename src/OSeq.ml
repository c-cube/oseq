
(** {1 OSeq: Functional Iterators} *)


(*$inject

  let plist f l = "["^String.concat ";" (List.map f l) ^"]"
  let ppair f1 f2 (x,y) = Printf.sprintf "(%s,%s)" (f1 x)(f2 y)
  let pint i = string_of_int i
  let pilist l = plist pint l
  let pilistlist l = plist (plist pint) l
  let pi2list l = plist (ppair pint pint) l
  let pstrlist l = plist (Printf.sprintf "%S") l
*)

type 'a t = unit -> 'a node

and 'a node =
  | Nil
  | Cons of 'a * 'a t

type 'a seq = 'a t (* alias *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

let empty () = Nil

let is_empty l = match l() with Nil -> true | Cons _ -> false

let return x () = Cons (x, empty)

let cons a b () = Cons (a,b)

let rec (--) i j () =
  if i=j then Cons (i, empty)
  else if i<j then Cons (i, i+1 -- j)
  else Cons (i, i-1--j)

(*$= & ~printer:pilist
  [0;1;2;3;4;5] (0-- 5 |> to_list)
  [0]           (0-- 0 |> to_list)
  [5;4;3;2]     (5-- 2 |> to_list)
*)

let (--^) i j =
  if i=j then empty
  else if i<j then i -- (j-1)
  else i -- (j+1)

(*$= & ~printer:pilist
  [1;2;3;4] (1 --^ 5 |> to_list) 
  [5;4;3;2] (5 --^ 1 |> to_list) 
  [1]       (1 --^ 2 |> to_list) 
  []        (0 --^ 0 |> to_list) 
*)

let rec map f l () =
  match l () with
  | Nil -> Nil
  | Cons (x,tail) -> Cons (f x, map f tail)

let rec fold_map f acc l () =
  match l () with
  | Nil -> Nil
  | Cons (x, tl) ->
    let acc = f acc x in
    Cons (acc, fold_map f acc tl)

let rec repeatedly f () = Cons (f(), repeatedly f)

let rec repeat x () = Cons (x, repeat x)

(*$T
  repeat 0 |> take 4 |> to_list = [0;0;0;0]
  repeat 1 |> take 0 |> to_list = []
*)


let init ?(n=max_int) f =
  let rec aux r () =
    if r >= n then Nil
    else (
      let x = f r in
      Cons (x, aux (r+1))
    )
  in
  aux 0

(*$T init
  init ~n:5 (fun i->i) |> to_list = [0;1;2;3;4]
*)

let mapi f l =
  let rec aux f l i () = match l() with
    | Nil -> Nil
    | Cons (x, tl) ->
      Cons (f i x, aux f tl (i+1))
  in
  aux f l 0

(*$T
  mapi (fun i x -> i,x) (1 -- 3) |> to_list = [0, 1; 1, 2; 2, 3]
*)

let rec filter_map f (l:'a t) () = match l() with
  | Nil -> Nil
  | Cons (x, l') ->
    begin match f x with
      | None -> filter_map f l' ()
      | Some y -> Cons (y, filter_map f l')
    end

(*$T
  filter_map (fun x -> if x mod 2=0 then Some (x*3) else None) (1--10) |> to_list \
    = [6;12;18;24;30]
*)


let filter f l =
  let rec aux f l () = match l () with
    | Nil -> Nil
    | Cons (x,tl) when f x -> Cons (x, aux f tl)
    | Cons (_, tl) -> aux f tl ()
  in
  aux f l

let rec append a b () =
  match a () with
  | Nil -> b ()
  | Cons (x,tl) -> Cons (x, append tl b)

let rec cycle l () = append l (cycle l) ()

let iterate x f =
  let rec aux f x () =
    let y = f x in
    Cons (x, aux f y)
  in
  aux f x

(*$T iterate
  iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]
*)


let rec fold f acc l = match l() with
  | Nil -> acc
  | Cons (x,tl) -> fold f (f acc x) tl

let reduce f g =
  match g() with
  | Nil -> invalid_arg "reduce"
  | Cons (x, tl) -> fold f x tl

let rec iter f l = match l () with
  | Nil -> ()
  | Cons (x, l') -> f x; iter f l'

let iteri f l =
  let rec aux f l i = match l() with
    | Nil -> ()
    | Cons (x, l') ->
      f i x;
      aux f l' (i+1)
  in
  aux f l 0

let length l = fold (fun acc _ -> acc+1) 0 l

(*$T
  cycle (of_list [1;2]) |> take 5 |> to_list = [1;2;1;2;1]
  cycle (of_list [1; ~-1]) |> take 100_000 |> fold (+) 0 = 0
*)

let rec unfold f acc () = match f acc with
  | None -> Nil
  | Some (x, acc') -> Cons (x, unfold f acc')

(*$T
  let f = function  10 -> None | x -> Some (x, x+1) in \
  unfold f 0 |> to_list = [0;1;2;3;4;5;6;7;8;9]
*)

let rec flat_map f l () =
  match l() with
  | Nil -> Nil
  | Cons (x,tl) ->
    fm_app_ f (f x) tl ()
and fm_app_ f l l' () = match l () with
  | Nil -> flat_map f l' ()
  | Cons (x, tl) -> Cons (x, fm_app_ f tl l')

(*$Q
  Q.(pair (fun1 Observable.int (small_list int)) (small_list int)) (fun (f, l) -> \
    (of_list l |> flat_map (fun x -> of_list (Q.Fn.apply f x)) |> to_list) \
    = CCList.flat_map (Q.Fn.apply f) l)
  Q.(pair (fun1 Observable.int (small_list int)) (small_list int)) (fun (f, l) -> \
    (of_list l |> flat_map (fun x -> of_list (Q.Fn.apply f x)) |> to_list) \
    = (of_list l |> map (Q.Fn.apply f) |> map of_list |> flatten |> to_list))
  *)

let take_nth n g =
  let rec aux i g () =
    match g() with
    | Nil -> Nil
    | Cons (_, tl) when i>0 -> aux (i-1) tl ()
    | Cons (x, tl) ->
      assert (i=0);
      Cons (x, aux (n-1) tl)
  in aux 0 g

let rec nth i l =
  match l() with
  | Nil -> raise Not_found
  | Cons (x, _) when i=0 -> x
  | Cons (_, tl) -> nth (i-1) tl

(*$= nth & ~printer:string_of_int
  4 (nth 4 (0--10))
  8 (nth 8 (0--10))
*)

(*$T
  (try ignore (nth 11 (1--10)); false with Not_found -> true)
*)

let mem ~eq x gen =
  let rec mem eq x gen =
    match gen() with
    | Nil -> false
    | Cons (y,tl) -> eq x y || mem eq x tl
  in mem eq x gen

let rec for_all p gen =
  match gen() with
  | Nil -> true
  | Cons (x,tl) -> p x && for_all p tl

let rec exists p gen =
  match gen() with
  | Nil -> false
  | Cons (x,tl) -> p x || exists p tl

let min ~lt gen =
  match gen () with
  | Cons (x,tl) ->
    fold (fun min x -> if lt x min then x else min) x tl
  | Nil -> invalid_arg "min"

(*$T
  min ~lt:(<) (of_list [1;4;6;0;11; -2]) = ~-2
  (try ignore (min ~lt:(<) empty); false with Invalid_argument _ -> true)
*)

let max ~lt gen =
  match gen () with
  | Cons (x,tl) ->
    fold (fun max x -> if lt max x then x else max) x tl
  | Nil -> invalid_arg "max"

(*$T
  max ~lt:(<) (of_list [1;4;6;0;11; -2]) = 11
  (try ignore (max ~lt:(<) empty); false with Invalid_argument _ -> true)
*)

let equal ~eq gen1 gen2 =
  let rec check gen1 gen2 =
    match gen1(), gen2() with
    | Nil, Nil -> true
    | Cons (x1,tl1), Cons (x2,tl2) when eq x1 x2 -> check tl1 tl2
    | _ -> false
  in
  check gen1 gen2

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    equal ~eq:Pervasives.(=) (of_list l1)(of_list l2) = (l1 = l2))
*)

(* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] *)
let partition p gen =
  filter p gen, filter (fun x -> not (p x)) gen

(*$T
  partition (fun x -> x mod 2 = 0) (1--10) |> \
    (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])
*)

let zip_index gen =
  let rec aux r gen () =
    match gen() with
    | Nil -> Nil
    | Cons (x, tl) -> Cons ((r,x), aux (r+1) tl)
  in
  aux 0 gen

(*$T
  zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]
*)


let rec map2 f l1 l2 () = match l1(), l2() with
  | Nil, _
  | _, Nil -> Nil
  | Cons(x1,l1'), Cons(x2,l2') ->
    Cons (f x1 x2, map2 f l1' l2')

let rec fold2 f acc l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> acc
  | Cons(x1,l1'), Cons(x2,l2') ->
    fold2 f (f acc x1 x2) l1' l2'

let rec iter2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> ()
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2; iter2 f l1' l2'

let rec for_all2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> true
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2 && for_all2 f l1' l2'

let rec exists2 f l1 l2 = match l1(), l2() with
  | Nil, _
  | _, Nil -> false
  | Cons(x1,l1'), Cons(x2,l2') ->
    f x1 x2 || exists2 f l1' l2'

let rec merge cmp l1 l2 () = match l1(), l2() with
  | Nil, tl2 -> tl2
  | tl1, Nil -> tl1
  | Cons(x1,l1'), Cons(x2,l2') ->
    if cmp x1 x2 < 0
    then Cons (x1, merge cmp l1' l2)
    else Cons (x2, merge cmp l1 l2')

let rec zip a b () = match a(), b() with
  | Nil, _
  | _, Nil -> Nil
  | Cons (x, a'), Cons (y, b') -> Cons ((x,y), zip a' b')

let unzip l =
  let rec first l () = match l() with
    | Nil -> Nil
    | Cons ((x,_), tl) -> Cons (x, first tl)
  and second l () = match l() with
    | Nil -> Nil
    | Cons ((_, y), tl) -> Cons (y, second tl)
  in
  first l, second l

(*$Q
  Q.(list (pair int int)) (fun l -> \
    let l = of_list l in let a, b = unzip l in equal (=) l (zip a b))
*)

let compare ~cmp gen1 gen2 : int =
  let rec aux gen1 gen2 =
    match gen1(), gen2() with
    | Nil, Nil -> 0
    | Cons (x1,tl1), Cons (x2,tl2) ->
        let c = cmp x1 x2 in
        if c <> 0 then c else aux tl1 tl2
    | Cons _, Nil -> 1
    | Nil, Cons _ -> -1
  in aux gen1 gen2

(*$Q
  (Q.pair (Q.list Q.small_int)(Q.list Q.small_int)) (fun (l1,l2) -> \
    let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in \
    sign (compare ~cmp:Pervasives.compare (of_list l1)(of_list l2)) = sign (Pervasives.compare l1 l2))
*)

let rec find p e = match e () with
  | Nil -> None
  | Cons (x,_) when p x -> Some x
  | Cons (_,tl) -> find p tl

(*$T
   find (fun x -> x>=5) (1--10) = Some 5
   find (fun x -> x>5) (1--4) = None
*)

let sum e = fold (+) 0 e

(*$T
  sum (1--10) = 55
*)

(** {2 Fair Combinations} *)

let rec interleave a b () = match a() with
  | Nil -> b ()
  | Cons (x, tail) -> Cons (x, interleave b tail)

let rec fair_flat_map f a () = match a() with
  | Nil -> Nil
  | Cons (x, tail) ->
    let y = f x in
    interleave y (fair_flat_map f tail) ()

let rec fair_app f a () = match f() with
  | Nil -> Nil
  | Cons (f1, fs) ->
    interleave (map f1 a) (fair_app fs a) ()

let rec flatten l () =
  match l() with
  | Nil -> Nil
  | Cons (x,tl) ->
    flat_app_ x tl ()
and flat_app_ l l' () = match l () with
  | Nil -> flatten l' ()
  | Cons (x, tl) -> Cons (x, flat_app_ tl l')

let rec take n (l:'a t) () =
  if n=0 then Nil
  else match l () with
    | Nil -> Nil
    | Cons (x,l') -> Cons (x, take (n-1) l')

let rec take_while p l () = match l () with
  | Nil -> Nil
  | Cons (x,l') ->
    if p x then Cons (x, take_while p l') else Nil

(*$T
  of_list [1;2;3;4] |> take_while (fun x->x < 4) |> to_list = [1;2;3]
*)


let rec drop n (l:'a t) () = match l () with
  | l' when n=0 -> l'
  | Nil -> Nil
  | Cons (_,l') -> drop (n-1) l' ()

let rec drop_while p l () = match l() with
  | Nil -> Nil
  | Cons (x,l') when p x -> drop_while p l' ()
  | Cons _ as res -> res

(*$Q
  (Q.pair (Q.list Q.small_int) Q.small_int) (fun (l,n) -> \
    let s = of_list l in let s1, s2 = take n s, drop n s in \
    append s1 s2 |> to_list = l  )
*)

let rec fold_while f acc gen =
  match gen() with
  | Nil -> acc
  | Cons (x, tl) ->
    let acc, cont = f acc x in
    match cont with
    | `Stop -> acc
    | `Continue -> fold_while f acc tl

(*$T
  fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0 \
    (of_list [true;true;false;true]) = 2
*)

let scan f acc g : _ t =
  let rec aux f acc g () =
    match g () with
    | Nil -> Cons (acc, empty)
    | Cons (x, tl) ->
      let acc' = f acc x in
      Cons (acc, aux f acc' tl)
  in
  aux f acc g

(*$T scan
  scan (fun acc x -> x+1::acc) [] (1--5) |> to_list \
    = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]
*)

let unfold_scan f acc g =
  let rec aux f acc g () =
    match g() with
    | Nil -> Nil
    | Cons (x, tl) ->
      let acc, res = f acc x in
      Cons (res, aux f acc tl)
  in
  aux f acc g

(*$T unfold_scan
  unfold_scan (fun acc x -> x+acc,acc) 0 (1--5) |> to_list \
    = [0; 1; 3; 6; 10]
*)

let product_with f l1 l2 =
  let rec next_left l1 l2 () = match l1() with
    | Nil -> Nil
    | Cons (x1, tl1) -> append_all ~tl1 ~l2_init:l2 x1 l2 ()
  and append_all ~tl1 ~l2_init x1 l2 () = match l2() with
    | Nil -> next_left tl1 l2_init ()
    | Cons (x2, tl2) ->
      Cons (f x1 x2, append_all ~tl1 ~l2_init x1 tl2)
  in
  next_left l1 l2

(*$Q
  Q.(pair (small_list int)(small_list int)) (fun (l1,l2) -> \
    let lsort=List.sort Pervasives.compare in \
    lsort (List.flatten@@List.map (fun x ->List.map (fun y->x,y) l2)l1) = \
    lsort (product (of_list l1)(of_list l2) |> to_list))
*)

let product l1 l2 =
  product_with (fun x y -> x,y) l1 l2

let app fs xs = product_with (fun f x -> f x) fs xs

module Infix = struct
  let (>>=) xs f = flat_map f xs
  let (>|=) xs f = map f xs
  let (>>|) xs f = map f xs
  let (<*>) = app
  let (>>-) a f = fair_flat_map f a
  let (<.>) f a = fair_app f a
  let (--) = (--)
  let (--^) = (--^)
end

include Infix

let rec group ~eq l () = match l() with
  | Nil -> Nil
  | Cons (x, l') ->
    Cons (cons x (take_while (eq x) l'), group ~eq (drop_while (eq x) l'))

(*$T
  of_list [1;1;1;2;2;3;3;1] |> group ~eq:(=) |> map to_list |> to_list = \
    [[1;1;1]; [2;2]; [3;3]; [1]]
*)

let rec uniq_rec_ eq prev l () = match prev, l() with
  | _, Nil -> Nil
  | None, Cons (x, l') ->
    Cons (x, uniq_rec_ eq (Some x) l')
  | Some y, Cons (x, l') ->
    if eq x y
    then uniq_rec_ eq prev l' ()
    else Cons (x, uniq_rec_ eq (Some x) l')

let uniq ~eq l = uniq_rec_ eq None l

let chunks n e =
  let rec aux e () =
    match e() with
    | Nil -> Nil
    | Cons (x,tl) ->
      let a = Array.make n x in
      fill a 1 tl

  and fill a i e =
    (* fill the array. [i]: current index to fill *)
    if i = n
    then Cons (a, aux e)
    else match e() with
      | Nil -> Cons (Array.sub a 0 i, empty)  (* last array is not full *)
      | Cons (x, tl) ->
          a.(i) <- x;
          fill a (i+1) tl
  in
  aux e

(*$T
  chunks 25 (0--100) |> map Array.to_list |> to_list = \
    List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]
*)

(* Put [x] between elements of [enum] *)
let intersperse x g =
  let rec aux_with_sep g () = match g() with
    | Nil -> Nil
    | Cons (y, g') ->
      Cons (x, cons y (aux_with_sep g'))
  in
  fun () -> match g() with
    | Nil -> Nil
    | Cons (x, g) -> Cons (x, aux_with_sep g)

(*$= & ~printer:pilist
  [] (intersperse 0 empty |> to_list)
  [1] (intersperse 0 (return 1) |> to_list)
  [1;0;2;0;3;0;4;0;5] (intersperse 0 (1--5) |> to_list)
*)

(* functional queue *)
module F_queue = struct
  type 'a t = {
    hd : 'a list;
    tl : 'a list;
  } (** Queue containing elements of type 'a *)

  let empty = {
    hd = [];
    tl = [];
  }

  (* invariant: if hd=[], then tl=[] *)
  let make_ hd tl = match hd with
    | [] -> {hd=List.rev tl; tl=[] }
    | _::_ -> {hd; tl; }

  let list_is_empty = function
    | [] -> true
    | _::_ -> false

  let is_empty q = list_is_empty q.hd

  let push x q = make_ q.hd (x :: q.tl)

  let pop_exn q =
    match q.hd with
    | [] -> assert (list_is_empty q.tl); invalid_arg "F_queue.pop_exn"
    | x::hd' ->
      let q' = make_ hd' q.tl in
      x, q'
end

let merge gens : _ t =
  (* recursive function to get next element
     @param q the already produced generators
     @param tl the generators still untouched *)
  let rec next (q:'a t F_queue.t) (tl:'a t t) () =
    match tl() with
    | Nil ->
      (* all generators are in [q] now *)
      if F_queue.is_empty q then Nil
      else (
        let g, q' = F_queue.pop_exn q in
        match g() with
        | Nil -> next q' empty ()
        | Cons (x, g') ->
          Cons (x, next (F_queue.push g' q') empty)
      )
    | Cons (g, tl') ->
      yield_from g q tl'

  and yield_from g q tl =
    match g() with
    | Nil -> next q tl ()
    | Cons (x, g') ->
      Cons (x, next (F_queue.push g' q) tl)
  in
  next F_queue.empty gens

(*$T
  merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]]) \
    |> to_list |> List.sort Pervasives.compare = [1;2;3;4;5;6;7;8;9]
*)

(*$R
  let e = of_list [1--3; 4--6; 7--9] in
  let e' = merge e in
  OUnit.assert_equal [1;2;3;4;5;6;7;8;9]
    (to_list e' |> List.sort Pervasives.compare);
*)

let intersection ~cmp gen1 gen2 : _ t =
  let rec next x1 x2 () =
    match x1, x2 with
    | Cons (y1,tl1), Cons (y2,tl2) ->
      let c = cmp y1 y2 in
      if c = 0  (* equal elements, yield! *)
      then Cons (y1, fun () -> next (tl1()) (tl2()) ())
      else if c < 0 (* drop y1 *)
      then next (tl1()) x2 ()
      else (* drop y2 *)
        next x1 (tl2()) ()
    | _ -> Nil
  in
  fun () -> next (gen1()) (gen2()) ()

(*$= & ~printer:pilist
  [1;2;4;8] (intersection ~cmp:Pervasives.compare \
    (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9]) |> to_list)
*)

let rec zip_with f a b () =
  match a(), b() with
  | Cons (xa,tla), Cons (xb,tlb) -> Cons (f xa xb, zip_with f tla tlb)
  | _ -> Nil

(*$Q
  (Q.list Q.small_int) (fun l -> \
    zip_with (fun x y->x,y) (of_list l) (of_list l) \
      |> unzip |> fst |> to_list = l)
*)

(*$R
  let e = zip_with (+) (repeat 1) (4--7) in
  OUnit.assert_equal [5;6;7;8] (to_list e);
*)


let sorted_merge ~cmp gen1 gen2 : _ t =
  let rec next x1 x2 () =
    match x1, x2 with
    | Nil, Nil -> Nil
    | Cons (y1, tl1), Cons (y2, tl2) ->
      if cmp y1 y2 <= 0
      then Cons (y1, next (tl1()) x2)
      else Cons (y2, next x1 (tl2()))
    | Cons _, Nil -> x1
    | Nil, Cons _ -> x2
  in
  fun () -> next (gen1()) (gen2()) ()

(*$T
  sorted_merge ~cmp:Pervasives.compare \
  (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11]) \
    |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]
*)


let round_robin ?(n=2) gen : _ t list =
  let rec start i =
    if i=n then []
    else (
      let g = take_nth n (drop i gen) in
      g :: start (i+1)
    )
  in
  start 0

(*$= & ~printer:pilistlist
  [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]] \
  (round_robin ~n:3 (1--12) |> List.map to_list)
*)

(*$R round_robin
  let e = round_robin ~n:2 (1--10) in
  match e with
  | [a;b] ->
    OUnit.assert_equal ~printer:pilist [1;3;5;7;9] (to_list a);
    OUnit.assert_equal ~printer:pilist [2;4;6;8;10] (to_list b)
  | _ -> OUnit.assert_failure "wrong list lenght"
*)

(*$R round_robin
  let e = round_robin ~n:3 (1 -- 999) in
  let l = List.map length e in
  OUnit.assert_equal ~printer:pilist [333;333;333] l;
*)


(** {2 Combinatorics} *)

(* state of the permutation machine. One machine manages one element [x],
   and depends on a deeper machine [g] that generates permutations of the
   list minus this element (down to the empty list).
   The machine can do two things:
    - insert the element in the current list of [g], at any position
    - obtain the next list of [g]
*)

let permutations l =
  let rec aux n l =
    match l with
    | [] -> assert (n=0); return []
    | x :: tail ->
      aux (n-1) tail >>= fun tail ->
      insert_ x [] tail
  (* insert [x] in [tail[i…n]] *)
  and insert_ x left right : _ t =
    match right with
    | [] -> return (List.rev (x::left))
    | y :: right' ->
      cons
        (List.rev_append left (x::right))
        (insert_ x (y::left) right')
  in
  aux (List.length l) l

(*$= permutations & ~printer:pilistlist
  [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]] \
  (permutations CCList.(1--3) |> to_list |> List.sort Pervasives.compare)
  [[]] (permutations [] |> to_list)
  [[1]] (permutations [1] |> to_list)
*)

let combinations n g =
  assert (n >= 0);
  let rec make_state n l () = match n, l() with
    | 0, _ -> Cons ([], empty)
    | _, Nil -> Nil
    | _, Cons (x,tail) ->
      let m1 = make_state (n-1) tail in
      let m2 = make_state n tail in
      add x m1 m2 ()
  and add x m1 m2 () = match m1 () with
    | Nil -> m2 ()
    | Cons (l, m1') -> Cons (x::l, add x m1' m2)
  in
  make_state n g

(*$= & ~printer:pilistlist
  [[1;2]; [1;3]; [1;4]; [2;3]; [2;4]; [3;4]] \
    (combinations 2 (1--4) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare)
  [[]] (combinations 0 (1--4) |> to_list)
  [[1]] (combinations 1 (return 1) |> to_list)
*)

let power_set g : _ t =
  let rec make_state l () = match l with
    | [] -> Cons ([], empty)
    | x::tail ->
      let m = make_state tail in
      add x m ()
  and add x m () = match m () with
    | Nil -> Nil
    | Cons (l, m') -> Cons (x :: l, cons l (add x m'))
  in
  let l = fold (fun acc x->x::acc) [] g in
  make_state l

(*$= & ~printer:pilistlist
  [[]; [1]; [1;2]; [1;2;3]; [1;3]; [2]; [2;3]; [3]] \
  (power_set (1--3) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare)
  [[]] (power_set empty |> to_list)
  [[]; [1]] (power_set (return 1) |> map (List.sort Pervasives.compare) \
    |> to_list |> List.sort Pervasives.compare)
*)


(** {2 Conversions} *)

let rec to_rev_list_rec_ acc l = match l() with
  | Nil -> acc
  | Cons (x,l') -> to_rev_list_rec_ (x::acc) l'

let to_rev_list l = to_rev_list_rec_ [] l

let to_list l =
  let rec direct i (l:'a t) = match l () with
    | Nil -> []
    | _ when i=0 -> List.rev (to_rev_list_rec_ [] l)
    | Cons (x, f) -> x :: direct (i-1) f
  in
  direct 200 l

let of_list l =
  let rec aux l () = match l with
    | [] -> Nil
    | x::l' -> Cons (x, aux l')
  in aux l

let of_array ?(start=0) ?len a =
  let len = match len with Some l -> l | None -> Array.length a - start in
  let rec aux a i () =
    if i=len then Nil
    else Cons (a.(i), aux a (i+1))
  in
  aux a start

let to_array l =
  match l() with
  | Nil -> [| |]
  | Cons (x, _) ->
    let n = length l in
    let a = Array.make n x in (* need first elem to create [a] *)
    iteri
      (fun i x -> a.(i) <- x)
      l;
    a

(*$Q
   Q.(array int) (fun a -> of_array a |> to_array = a)
*)

(*$T
  of_array [| 1; 2; 3 |] |> to_list = [1;2;3]
  of_list [1;2;3] |> to_array = [| 1; 2; 3; |]
*)

let to_buffer buf g = iter (Buffer.add_char buf) g

let of_string ?(start=0) ?len s =
  let len = match len with
    | None -> String.length s - start
    | Some n -> assert (n + start < String.length s); n in
  let rec aux i () =
    if i >= start + len
    then Nil
    else (
      let x = s.[i] in
      Cons (x, aux (i+1))
    )
  in
  aux 0

let to_string s =
  let buf = Buffer.create 16 in
  to_buffer buf s;
  Buffer.contents buf

let rec to_seq res k = match res () with
  | Nil -> ()
  | Cons (s, f) -> k s; to_seq f k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l () with
    | Nil -> None
    | Cons (x,l') ->
      l := l';
      Some x

type 'a of_gen_state =
  | Of_gen_thunk of 'a gen
  | Of_gen_saved of 'a node

let of_gen g =
  let rec consume r () = match !r with
    | Of_gen_saved cons -> cons
    | Of_gen_thunk g ->
      begin match g() with
        | None ->
          r := Of_gen_saved Nil;
          Nil
        | Some x ->
          let tl = consume (ref (Of_gen_thunk g)) in
          let l = Cons (x, tl) in
          r := Of_gen_saved l;
          l
      end
  in
  consume (ref (Of_gen_thunk g))

(*$R
  let g = let n = ref 0 in fun () -> Some (incr n; !n) in
  let l = of_gen g in
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [1;2;3;4;5;6;7;8;9;10] (take 10 l |> to_list);
  assert_equal [11;12] (drop 10 l |> take 2 |> to_list);
*)

let rec of_gen_transient f () =
  match f() with
  | None -> Nil
  | Some x -> Cons (x, of_gen_transient f)

let sort ~cmp l =
  let l = to_list l in
  of_list (List.sort cmp l)

let sort_uniq ~cmp l =
  let l = to_list l in
  uniq (fun x y -> cmp x y = 0) (of_list (List.sort cmp l))

let lines g : _ t =
  let rec aux g buf () =
    match g() with
    | Nil ->
      (* only return a non-empty line *)
      if Buffer.length buf = 0
      then Nil
      else (
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Cons (s, empty)
      )
    | Cons (c, tl) ->
      if c = '\n' then (
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Cons (s, aux tl buf)
      ) else (
        Buffer.add_char buf c;
        aux tl buf ()
      )
  in
  aux g (Buffer.create 16)

(*$= & ~printer:Q.Print.(list string)
  ["abc"; "de"; ""] (lines (of_string "abc\nde\n\n") |> to_list)
*)

let unlines g : _ t =
  let rec aux g st () =
    match st with
    | `Stop -> Nil
    | `Next ->
      begin match g() with
        | Nil -> Nil
        | Cons ("",tl) -> Cons ('\n', aux tl st) (* empty line *)
        | Cons (s,tl) -> Cons (s.[0], aux tl (`Consume (s,1)))
      end
    | `Consume (s, i) when i=String.length s ->
      Cons ('\n', aux g `Next)
    | `Consume (s, i) ->
      Cons (s.[i], aux g (`Consume (s,i+1)))
  in
  aux g `Next

(*$Q
  Q.printable_string (fun s -> \
    of_string s |> lines |> unlines |> to_string |> String.trim = String.trim s)
*)

type 'a memoize =
  | MemoThunk
  | MemoSave of 'a node

let rec memoize f =
  let r = ref MemoThunk in
  fun () -> match !r with
    | MemoSave l -> l
    | MemoThunk ->
      let l = match f() with
        | Nil -> Nil
        | Cons (x, tail) -> Cons (x, memoize tail)
      in
      r := MemoSave l;
      l

module Generator = struct
  type 'a t =
    | Skip
    | Yield of 'a
    | Delay of (unit -> 'a t)
    | Append of 'a t * 'a t

  let empty = Skip

  let yield x = Yield x

  let (>>=) x f = Append (x,Delay f)

  let delay f = Delay f

  let run (x:'a t) : 'a seq =
    let rec aux l () = match l with
      | [] -> Nil
      | Skip :: tl -> aux tl ()
      | Yield x :: tl -> Cons (x, aux tl)
      | Delay f :: tl -> aux (f () :: tl) ()
      | Append (x1, x2) :: tl -> aux (x1 :: x2 :: tl) ()
    in
    aux [x]
end

(*$R
  let naturals =
    Generator.(let rec aux n = yield n>>= fun () -> aux (n+1) in run (aux 0))
  in
  let naturals' = unfold (fun n -> Some (n,n+1)) 0 in
  assert_equal ~printer:Q.Print.(list int)
    (take 100 naturals' |> to_list) (take 100 naturals |> to_list)
*)

(*$QR
  Q.(small_list int) (fun l ->
    let seq = of_list l in
    let seq2 =
      let open Generator in
      let rec aux seq = match seq() with
        | Nil -> empty
        | Cons (x, tl) -> yield x >>= fun () -> aux tl
      in
      run (aux seq)
    in
    equal Pervasives.(=) seq seq2)
*)

module IO = struct
  let with_file_in ?(mode=0o644) ?(flags=[]) filename f =
    let ic = open_in_gen flags mode filename in
    try
      let x = f ic in
      close_in_noerr ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let with_in ?mode ?flags filename f =
    with_file_in ?mode ?flags filename
      (fun ic ->
         f @@ of_gen @@
           (fun () ->
              try Some (input_char ic)
              with End_of_file -> None)
      )

  let with_lines ?mode ?flags filename f =
    with_file_in ?mode ?flags filename
      (fun ic ->
        f @@ of_gen @@ fun () ->
           try Some (input_line ic)
           with End_of_file -> None
      )

  let with_file_out ?(mode=0o644) ?(flags=[Open_creat;Open_wronly]) filename f =
    let oc = open_out_gen flags mode filename in
    try
      let x = f oc in
      close_out oc;
      x
    with e ->
      close_out_noerr oc;
      raise e

  let write_str ?mode ?flags ?(sep="") filename g =
    with_file_out ?mode ?flags filename
      (fun oc ->
         iteri
           (fun i s ->
              if i>0 then output_string oc sep;
              output_string oc s)
           g)

  let write ?mode ?flags filename g =
    with_file_out ?mode ?flags filename
      (fun oc ->
         iter (fun c -> output_char oc c) g
      )

  let write_lines ?mode ?flags filename g =
    with_file_out ?mode ?flags filename
      (fun oc ->
         iter (fun s -> output_string oc s; output_char oc '\n') g
      )
end

module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux acc l = match l () with
      | Nil -> return (of_list (List.rev acc))
      | Cons (x,l') ->
        f x >>= fun x' ->
        aux (x' :: acc) l'
    in
    aux [] l

  let sequence_m l = map_m (fun x->x) l

  let rec fold_m f acc l = match l() with
    | Nil -> return acc
    | Cons (x,l') ->
      f acc x >>= fun acc' -> fold_m f acc' l'
end

let pp ?(sep=",") pp_item fmt l =
  let rec pp fmt l = match l() with
    | Nil -> ()
    | Cons (x,l') ->
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      pp_item fmt x;
      pp fmt l'
  in
  match l() with
    | Nil -> ()
    | Cons (x,l') -> pp_item fmt x; pp fmt l'
