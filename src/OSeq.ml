open Seq

type 'a seq = 'a Seq.t (* alias *)
type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

let empty () = Nil

let is_empty l =
  match l () with
  | Nil -> true
  | Cons _ -> false

let return x () = Cons (x, empty)
let cons a b () = Cons (a, b)

let head_exn g =
  match g () with
  | Cons (x, _) -> x
  | Nil -> invalid_arg "OSeq.head_exn"

let tail_exn g : _ t =
  match g () with
  | Cons (_, l) -> l
  | Nil -> invalid_arg "OSeq.tail_exn"

let rec ( -- ) i j () =
  if i = j then
    Cons (i, empty)
  else if i < j then
    Cons (i, i + 1 -- j)
  else
    Cons (i, i - 1 -- j)

let ( --^ ) i j =
  if i = j then
    empty
  else if i < j then
    i -- (j - 1)
  else
    i -- (j + 1)

let rec map f l () =
  match l () with
  | Nil -> Nil
  | Cons (x, tail) -> Cons (f x, map f tail)

let rec fold_map f acc l () =
  match l () with
  | Nil -> Nil
  | Cons (x, tl) ->
    let acc = f acc x in
    Cons (acc, fold_map f acc tl)

let rec repeatedly f () = Cons (f (), repeatedly f)
let rec repeat x () = Cons (x, repeat x)

let init n f =
  let rec aux r () =
    if r >= n then
      Nil
    else (
      let x = f r in
      Cons (x, aux (r + 1))
    )
  in
  aux 0

let mapi f l =
  let rec aux f l i () =
    match l () with
    | Nil -> Nil
    | Cons (x, tl) -> Cons (f i x, aux f tl (i + 1))
  in
  aux f l 0

let rec filter_map f (l : 'a t) () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    (match f x with
    | None -> filter_map f l' ()
    | Some y -> Cons (y, filter_map f l'))

let filter f l =
  let rec aux f l () =
    match l () with
    | Nil -> Nil
    | Cons (x, tl) when f x -> Cons (x, aux f tl)
    | Cons (_, tl) -> aux f tl ()
  in
  aux f l

let rec append a b () =
  match a () with
  | Nil -> b ()
  | Cons (x, tl) -> Cons (x, append tl b)

let rec cycle l () = append l (cycle l) ()

let iterate f x =
  let rec aux f x () =
    let y = f x in
    Cons (x, aux f y)
  in
  aux f x

let rec fold f acc l =
  match l () with
  | Nil -> acc
  | Cons (x, tl) -> fold f (f acc x) tl

let fold_left = fold

let foldi f acc l =
  let rec foldi f i acc l =
    match l () with
    | Nil -> acc
    | Cons (x, tl) -> foldi f (succ i) (f i acc x) tl
  in
  foldi f 0 acc l

let reduce f g =
  match g () with
  | Nil -> invalid_arg "reduce"
  | Cons (x, tl) -> fold f x tl

let rec iter f l =
  match l () with
  | Nil -> ()
  | Cons (x, l') ->
    f x;
    iter f l'

let iteri f l =
  let rec aux f l i =
    match l () with
    | Nil -> ()
    | Cons (x, l') ->
      f i x;
      aux f l' (i + 1)
  in
  aux f l 0

let length l = fold (fun acc _ -> acc + 1) 0 l

let rec unfold f acc () =
  match f acc with
  | None -> Nil
  | Some (x, acc') -> Cons (x, unfold f acc')

let rec flat_map f l () =
  match l () with
  | Nil -> Nil
  | Cons (x, tl) -> fm_app_ f (f x) tl ()

and fm_app_ f l l' () =
  match l () with
  | Nil -> flat_map f l' ()
  | Cons (x, tl) -> Cons (x, fm_app_ f tl l')

let take_nth n g =
  let rec aux i g () =
    match g () with
    | Nil -> Nil
    | Cons (_, tl) when i > 0 -> aux (i - 1) tl ()
    | Cons (x, tl) ->
      assert (i = 0);
      Cons (x, aux (n - 1) tl)
  in
  aux 0 g

let rec nth i l =
  match l () with
  | Nil -> raise Not_found
  | Cons (x, _) when i = 0 -> x
  | Cons (_, tl) -> nth (i - 1) tl

let mem eq x gen =
  let rec mem eq x gen =
    match gen () with
    | Nil -> false
    | Cons (y, tl) -> eq x y || mem eq x tl
  in
  mem eq x gen

let rec for_all p gen =
  match gen () with
  | Nil -> true
  | Cons (x, tl) -> p x && for_all p tl

let rec exists p gen =
  match gen () with
  | Nil -> false
  | Cons (x, tl) -> p x || exists p tl

let min ~lt gen =
  match gen () with
  | Cons (x, tl) ->
    fold
      (fun min x ->
        if lt x min then
          x
        else
          min)
      x tl
  | Nil -> invalid_arg "min"

let max ~lt gen =
  match gen () with
  | Cons (x, tl) ->
    fold
      (fun max x ->
        if lt max x then
          x
        else
          max)
      x tl
  | Nil -> invalid_arg "max"

let equal eq gen1 gen2 =
  let rec check gen1 gen2 =
    match gen1 (), gen2 () with
    | Nil, Nil -> true
    | Cons (x1, tl1), Cons (x2, tl2) when eq x1 x2 -> check tl1 tl2
    | _ -> false
  in
  check gen1 gen2

(* [partition p l] returns the elements that satisfy [p],
   and the elements that do not satisfy [p] *)
let partition p gen = filter p gen, filter (fun x -> not (p x)) gen

let zip_index gen =
  let rec aux r gen () =
    match gen () with
    | Nil -> Nil
    | Cons (x, tl) -> Cons ((r, x), aux (r + 1) tl)
  in
  aux 0 gen

let rec map2 f l1 l2 () =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> Nil
  | Cons (x1, l1'), Cons (x2, l2') -> Cons (f x1 x2, map2 f l1' l2')

let rec fold2 f acc l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> acc
  | Cons (x1, l1'), Cons (x2, l2') -> fold2 f (f acc x1 x2) l1' l2'

let rec iter2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> ()
  | Cons (x1, l1'), Cons (x2, l2') ->
    f x1 x2;
    iter2 f l1' l2'

let rec for_all2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> true
  | Cons (x1, l1'), Cons (x2, l2') -> f x1 x2 && for_all2 f l1' l2'

let rec exists2 f l1 l2 =
  match l1 (), l2 () with
  | Nil, _ | _, Nil -> false
  | Cons (x1, l1'), Cons (x2, l2') -> f x1 x2 || exists2 f l1' l2'

let rec zip a b () =
  match a (), b () with
  | Nil, _ | _, Nil -> Nil
  | Cons (x, a'), Cons (y, b') -> Cons ((x, y), zip a' b')

let unzip l =
  let rec first l () =
    match l () with
    | Nil -> Nil
    | Cons ((x, _), tl) -> Cons (x, first tl)
  and second l () =
    match l () with
    | Nil -> Nil
    | Cons ((_, y), tl) -> Cons (y, second tl)
  in
  first l, second l

let compare cmp gen1 gen2 : int =
  let rec aux gen1 gen2 =
    match gen1 (), gen2 () with
    | Nil, Nil -> 0
    | Cons (x1, tl1), Cons (x2, tl2) ->
      let c = cmp x1 x2 in
      if c <> 0 then
        c
      else
        aux tl1 tl2
    | Cons _, Nil -> 1
    | Nil, Cons _ -> -1
  in
  aux gen1 gen2

let rec find p e =
  match e () with
  | Nil -> None
  | Cons (x, _) when p x -> Some x
  | Cons (_, tl) -> find p tl

let rec find_map f e =
  match e () with
  | Nil -> None
  | Cons (x, tl) ->
    (match f x with
    | None -> find_map f tl
    | Some _ as res -> res)

let sum e = fold ( + ) 0 e

(** {2 Fair Combinations} *)

let rec interleave a b () =
  match a () with
  | Nil -> b ()
  | Cons (x, tail) -> Cons (x, interleave b tail)

let rec flat_map_interleave f a () =
  match a () with
  | Nil -> Nil
  | Cons (x, tail) ->
    let y = f x in
    interleave y (flat_map_interleave f tail) ()

let rec app_interleave f a () =
  match f () with
  | Nil -> Nil
  | Cons (f1, fs) -> interleave (map f1 a) (app_interleave fs a) ()

let rec flatten l () =
  match l () with
  | Nil -> Nil
  | Cons (x, tl) -> flat_app_ x tl ()

and flat_app_ l l' () =
  match l () with
  | Nil -> flatten l' ()
  | Cons (x, tl) -> Cons (x, flat_app_ tl l')

let rec take n (l : 'a t) () =
  if n = 0 then
    Nil
  else (
    match l () with
    | Nil -> Nil
    | Cons (x, l') -> Cons (x, take (n - 1) l')
  )

let rec take_while p l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    if p x then
      Cons (x, take_while p l')
    else
      Nil

let rec drop n (l : 'a t) () =
  match l () with
  | l' when n = 0 -> l'
  | Nil -> Nil
  | Cons (_, l') -> drop (n - 1) l' ()

let rec drop_while p l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') when p x -> drop_while p l' ()
  | Cons _ as res -> res

let rec fold_while f acc gen =
  match gen () with
  | Nil -> acc
  | Cons (x, tl) ->
    let acc, cont = f acc x in
    (match cont with
    | `Stop -> acc
    | `Continue -> fold_while f acc tl)

let scan f acc g : _ t =
  let rec aux f acc g () =
    match g () with
    | Nil -> Cons (acc, empty)
    | Cons (x, tl) ->
      let acc' = f acc x in
      Cons (acc, aux f acc' tl)
  in
  aux f acc g

let unfold_scan f acc g =
  let rec aux f acc g () =
    match g () with
    | Nil -> Nil
    | Cons (x, tl) ->
      let acc, res = f acc x in
      Cons (res, aux f acc tl)
  in
  aux f acc g

let product_with f l1 l2 =
  (* take next element from [l1] *)
  let rec loop l1 () =
    match l1 () with
    | Nil -> Nil
    | Cons (x1, tl1) ->
      let seq = interleave (map (fun x2 -> f x1 x2) l2) (loop tl1) in
      seq ()
  in
  loop l1

let product l1 l2 = product_with (fun x y -> x, y) l1 l2
let app fs xs = product_with (fun f x -> f x) fs xs

module Infix = struct
  let[@inline] ( >>= ) xs f = flat_map f xs
  let[@inline] ( >|= ) xs f = map f xs
  let[@inline] ( >>| ) xs f = map f xs
  let ( <*> ) = app
  let ( -- ) = ( -- )
  let ( --^ ) = ( --^ )
  let[@inline] ( let+ ) x f = map f x
  let[@inline] ( let* ) x f = flat_map f x
  let ( and+ ) = product
  let ( and* ) = product
end

include Infix

let product3 l1 l2 l3 =
  (fun x1 x2 x3 -> x1, x2, x3) |> return <*> l1 <*> l2 <*> l3

let product4 l1 l2 l3 l4 =
  (fun x1 x2 x3 x4 -> x1, x2, x3, x4) |> return <*> l1 <*> l2 <*> l3 <*> l4

let product5 l1 l2 l3 l4 l5 =
  (fun x1 x2 x3 x4 x5 -> x1, x2, x3, x4, x5)
  |> return <*> l1 <*> l2 <*> l3 <*> l4 <*> l5

let product6 l1 l2 l3 l4 l5 l6 =
  (fun x1 x2 x3 x4 x5 x6 -> x1, x2, x3, x4, x5, x6)
  |> return <*> l1 <*> l2 <*> l3 <*> l4 <*> l5 <*> l6

let product7 l1 l2 l3 l4 l5 l6 l7 =
  (fun x1 x2 x3 x4 x5 x6 x7 -> x1, x2, x3, x4, x5, x6, x7)
  |> return <*> l1 <*> l2 <*> l3 <*> l4 <*> l5 <*> l6 <*> l7

let rec cartesian_product l () =
  match l () with
  | Nil -> Cons ([], empty)
  | Cons (l1, tail) ->
    let tail = cartesian_product tail in
    product_with (fun x tl -> x :: tl) l1 tail ()

(* cartesian product of lists of lists *)
let map_product_l f l =
  let l = map f l in
  cartesian_product l

let rec group eq l () =
  match l () with
  | Nil -> Nil
  | Cons (x, l') ->
    Cons (cons x (take_while (eq x) l'), group eq (drop_while (eq x) l'))

let rec uniq_rec_ eq prev l () =
  match prev, l () with
  | _, Nil -> Nil
  | None, Cons (x, l') -> Cons (x, uniq_rec_ eq (Some x) l')
  | Some y, Cons (x, l') ->
    if eq x y then
      uniq_rec_ eq prev l' ()
    else
      Cons (x, uniq_rec_ eq (Some x) l')

let uniq eq l = uniq_rec_ eq None l

let chunks n e =
  let rec aux e () =
    match e () with
    | Nil -> Nil
    | Cons (x, tl) ->
      let a = Array.make n x in
      fill a 1 tl
  and fill a i e =
    (* fill the array. [i]: current index to fill *)
    if i = n then
      Cons (a, aux e)
    else (
      match e () with
      | Nil -> Cons (Array.sub a 0 i, empty) (* last array is not full *)
      | Cons (x, tl) ->
        a.(i) <- x;
        fill a (i + 1) tl
    )
  in
  aux e

(* Put [x] between elements of [enum] *)
let intersperse x g =
  let rec aux_with_sep g () =
    match g () with
    | Nil -> Nil
    | Cons (y, g') -> Cons (x, cons y (aux_with_sep g'))
  in
  fun () ->
    match g () with
    | Nil -> Nil
    | Cons (x, g) -> Cons (x, aux_with_sep g)

(* functional queue *)
module F_queue = struct
  type 'a t = { hd: 'a list; tl: 'a list }
  (** Queue containing elements of type 'a *)

  let empty = { hd = []; tl = [] }

  (* invariant: if hd=[], then tl=[] *)
  let make_ hd tl =
    match hd with
    | [] -> { hd = List.rev tl; tl = [] }
    | _ :: _ -> { hd; tl }

  let list_is_empty = function
    | [] -> true
    | _ :: _ -> false

  let is_empty q = list_is_empty q.hd
  let push x q = make_ q.hd (x :: q.tl)

  let pop_exn q =
    match q.hd with
    | [] ->
      assert (list_is_empty q.tl);
      invalid_arg "F_queue.pop_exn"
    | x :: hd' ->
      let q' = make_ hd' q.tl in
      x, q'
end

type 'a merge_op = Merge_from of 'a t | Merge_start of 'a t t

let merge gens : _ t =
  (* recursive function to get next element
     @param q the already produced generators
     @param tl the generators still untouched *)
  let rec next (q : 'a merge_op F_queue.t) () =
    if F_queue.is_empty q then
      Nil
    else (
      match F_queue.pop_exn q with
      | Merge_from g, q' -> yield_from g q'
      | Merge_start gens, q' ->
        (match gens () with
        | Nil -> next q' ()
        | Cons (g, gens') ->
          let q' = F_queue.push (Merge_start gens') q' in
          yield_from g q')
    )
  and yield_from g q =
    match g () with
    | Nil -> next q ()
    | Cons (x, g') -> Cons (x, next (F_queue.push (Merge_from g') q))
  in
  let q = F_queue.push (Merge_start gens) F_queue.empty in
  next q

let intersection cmp gen1 gen2 : _ t =
  let rec next x1 x2 () =
    match x1, x2 with
    | Cons (y1, tl1), Cons (y2, tl2) ->
      let c = cmp y1 y2 in
      if c = 0 (* equal elements, yield! *) then
        Cons (y1, fun () -> next (tl1 ()) (tl2 ()) ())
      else if c < 0 (* drop y1 *) then
        next (tl1 ()) x2 ()
      else
        (* drop y2 *)
        next x1 (tl2 ()) ()
    | _ -> Nil
  in
  fun () -> next (gen1 ()) (gen2 ()) ()

let rec zip_with f a b () =
  match a (), b () with
  | Cons (xa, tla), Cons (xb, tlb) -> Cons (f xa xb, zip_with f tla tlb)
  | _ -> Nil

let sorted_merge cmp gen1 gen2 : _ t =
  let rec next x1 x2 () =
    match x1, x2 with
    | Nil, Nil -> Nil
    | Cons (y1, tl1), Cons (y2, tl2) ->
      if cmp y1 y2 <= 0 then
        Cons (y1, next (tl1 ()) x2)
      else
        Cons (y2, next x1 (tl2 ()))
    | Cons _, Nil -> x1
    | Nil, Cons _ -> x2
  in
  fun () -> next (gen1 ()) (gen2 ()) ()

let round_robin ?(n = 2) gen : _ t list =
  let rec start i =
    if i = n then
      []
    else (
      let g = take_nth n (drop i gen) in
      g :: start (i + 1)
    )
  in
  start 0

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
    | [] ->
      assert (n = 0);
      return []
    | x :: tail -> aux (n - 1) tail >>= fun tail -> insert_ x [] tail
  (* insert [x] in [tail[i…n]] *)
  and insert_ x left right : _ t =
    match right with
    | [] -> return (List.rev (x :: left))
    | y :: right' ->
      cons (List.rev_append left (x :: right)) (insert_ x (y :: left) right')
  in
  aux (List.length l) l

let combinations n g =
  assert (n >= 0);
  let rec make_state n l () =
    match n, l () with
    | 0, _ -> Cons ([], empty)
    | _, Nil -> Nil
    | _, Cons (x, tail) ->
      let m1 = make_state (n - 1) tail in
      let m2 = make_state n tail in
      add x m1 m2 ()
  and add x m1 m2 () =
    match m1 () with
    | Nil -> m2 ()
    | Cons (l, m1') -> Cons (x :: l, add x m1' m2)
  in
  make_state n g

let power_set g : _ t =
  let rec make_state l () =
    match l with
    | [] -> Cons ([], empty)
    | x :: tail ->
      let m = make_state tail in
      add x m ()
  and add x m () =
    match m () with
    | Nil -> Nil
    | Cons (l, m') -> Cons (x :: l, cons l (add x m'))
  in
  let l = fold (fun acc x -> x :: acc) [] g in
  make_state l

(** {2 Conversions} *)

let rec to_rev_list_rec_ acc l =
  match l () with
  | Nil -> acc
  | Cons (x, l') -> to_rev_list_rec_ (x :: acc) l'

let to_rev_list l = to_rev_list_rec_ [] l

let to_list l =
  let rec direct i (l : 'a t) =
    match l () with
    | Nil -> []
    | _ when i = 0 -> List.rev (to_rev_list_rec_ [] l)
    | Cons (x, f) -> x :: direct (i - 1) f
  in
  direct 200 l

let of_list l =
  let rec aux l () =
    match l with
    | [] -> Nil
    | x :: l' -> Cons (x, aux l')
  in
  aux l

let of_array ?(start = 0) ?len a =
  let len =
    match len with
    | Some l -> l
    | None -> Array.length a - start
  in
  let rec aux a i () =
    if i = len then
      Nil
    else
      Cons (a.(i), aux a (i + 1))
  in
  aux a start

let to_array l =
  match l () with
  | Nil -> [||]
  | Cons (x, _) ->
    let n = length l in
    let a = Array.make n x in
    (* need first elem to create [a] *)
    iteri (fun i x -> a.(i) <- x) l;
    a

let to_buffer buf g = iter (Buffer.add_char buf) g

let of_string ?(start = 0) ?len s =
  let len =
    match len with
    | None -> String.length s - start
    | Some n ->
      assert (n + start < String.length s);
      n
  in
  let rec aux i () =
    if i >= start + len then
      Nil
    else (
      let x = s.[i] in
      Cons (x, aux (i + 1))
    )
  in
  aux 0

let to_string s =
  let buf = Buffer.create 16 in
  to_buffer buf s;
  Buffer.contents buf

let concat_string ~sep s =
  match s () with
  | Nil -> ""
  | Cons (x, tl) ->
    let sep_len = String.length sep in
    let len =
      fold (fun len s -> String.length s + sep_len + len) (String.length x) tl
    in
    let bytes = Bytes.make len '\000' in
    let (_ : int) =
      fold
        (fun off s ->
          let slen = String.length s in
          assert (off + slen <= len);
          Bytes.unsafe_blit (Bytes.unsafe_of_string s) 0 bytes off slen;
          if off + slen < len then (
            (* not the last chunk *)
            Bytes.unsafe_blit
              (Bytes.unsafe_of_string sep)
              0 bytes (off + slen) sep_len;
            off + slen + sep_len
          ) else
            off + slen)
        0 s
    in
    Bytes.unsafe_to_string bytes

let rec to_iter res k =
  match res () with
  | Nil -> ()
  | Cons (s, f) ->
    k s;
    to_iter f k

let to_gen l =
  let l = ref l in
  fun () ->
    match !l () with
    | Nil -> None
    | Cons (x, l') ->
      l := l';
      Some x

type 'a of_gen_state = Of_gen_thunk of 'a gen | Of_gen_saved of 'a node

let of_gen g =
  let rec consume r () =
    match !r with
    | Of_gen_saved cons -> cons
    | Of_gen_thunk g ->
      (match g () with
      | None ->
        r := Of_gen_saved Nil;
        Nil
      | Some x ->
        let tl = consume (ref (Of_gen_thunk g)) in
        let l = Cons (x, tl) in
        r := Of_gen_saved l;
        l)
  in
  consume (ref (Of_gen_thunk g))

let rec of_gen_transient f () =
  match f () with
  | None -> Nil
  | Some x -> Cons (x, of_gen_transient f)

let sort cmp l =
  let l = to_list l in
  of_list (List.sort cmp l)

let sort_uniq cmp l =
  let l = to_list l in
  uniq (fun x y -> cmp x y = 0) (of_list (List.sort cmp l))

let lines g : _ t =
  let rec aux g buf () =
    match g () with
    | Nil ->
      (* only return a non-empty line *)
      if Buffer.length buf = 0 then
        Nil
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

let unlines g : _ t =
  let rec aux g st () =
    match st with
    | `Stop -> Nil
    | `Next ->
      (match g () with
      | Nil -> Nil
      | Cons ("", tl) -> Cons ('\n', aux tl st) (* empty line *)
      | Cons (s, tl) -> Cons (s.[0], aux tl (`Consume (s, 1))))
    | `Consume (s, i) when i = String.length s -> Cons ('\n', aux g `Next)
    | `Consume (s, i) -> Cons (s.[i], aux g (`Consume (s, i + 1)))
  in
  aux g `Next

type 'a memoize = MemoThunk | MemoSave of 'a node | MemoExn of exn

let rec memoize f =
  let r = ref MemoThunk in
  fun () ->
    match !r with
    | MemoSave l -> l
    | MemoExn e -> raise e
    | MemoThunk ->
      (try
         let l =
           match f () with
           | Nil -> Nil
           | Cons (x, tail) -> Cons (x, memoize tail)
         in
         r := MemoSave l;
         l
       with e ->
         r := MemoExn e;
         raise e)

module Generator = struct
  type 'a t =
    | Skip
    | Yield of 'a
    | Delay of (unit -> 'a t)
    | Append of 'a t * 'a t

  let empty = Skip
  let yield x = Yield x
  let ( >>= ) x f = Append (x, Delay f)
  let delay f = Delay f

  let run (x : 'a t) : 'a seq =
    let rec aux l () =
      match l with
      | [] -> Nil
      | Skip :: tl -> aux tl ()
      | Yield x :: tl -> Cons (x, aux tl)
      | Delay f :: tl -> aux (f () :: tl) ()
      | Append (x1, x2) :: tl -> aux (x1 :: x2 :: tl) ()
    in
    aux [ x ]
end

module type HashedType = Hashtbl.HashedType

let group_by_fold (type k) (module K : HashedType with type t = k) ~project
    ~fold ~init seq =
  let module Tbl = Hashtbl.Make (K) in
  (* compute group table *)
  let tbl =
    lazy
      (let tbl = Tbl.create 32 in
       iter
         (fun x ->
           let key = project x in
           let acc = try Tbl.find tbl key with Not_found -> init in
           let acc = fold acc x in
           Tbl.replace tbl key acc)
         seq;
       Tbl.to_seq tbl)
  in
  (* delay start *)
  fun () -> (Lazy.force tbl) ()

let group_by key ~project seq =
  group_by_fold key ~project ~fold:(fun l x -> x :: l) ~init:[] seq

let group_count key seq =
  group_by_fold key ~project:(fun x -> x) ~fold:(fun n _x -> n + 1) ~init:0 seq

let join_by (type k) (module Key : HashedType with type t = k) ~project_left
    ~project_right ~merge seq1 seq2 : _ t =
  let module Tbl = Hashtbl.Make (Key) in
  let tbl_left = Tbl.create 16 in
  let tbl_right = Tbl.create 16 in

  let seq1 = ref seq1 in
  let seq2 = ref seq2 in

  let get_l tbl k = try Tbl.find tbl k with Not_found -> [] in

  let next_left = ref true in
  let q = Queue.create () in

  let rec gen () =
    match Queue.take q with
    | x -> Some x
    | exception Queue.Empty ->
      if !next_left then (
        next_left := false;
        match !seq1 () with
        | Nil -> ()
        | Cons (x, tl1) ->
          seq1 := tl1;
          let key = project_left x in
          Tbl.replace tbl_left key (x :: get_l tbl_left key);
          (* join [x] with the RHS items that have the same key *)
          let ys = get_l tbl_right key in
          List.iter
            (fun y ->
              match merge key x y with
              | None -> ()
              | Some r -> Queue.push r q)
            ys
      ) else (
        next_left := true;
        match !seq2 () with
        | Nil -> ()
        | Cons (y, tl2) ->
          seq2 := tl2;
          let key = project_right y in
          Tbl.replace tbl_right key (y :: get_l tbl_right key);
          (* join [y] with the LHS items that have the same key *)
          let xs = get_l tbl_left key in
          List.iter
            (fun x ->
              match merge key x y with
              | None -> ()
              | Some r -> Queue.push r q)
            xs
      );
      gen ()
  in
  memoize (of_gen_transient gen)

let join_by_fold (type k) (module Key : HashedType with type t = k)
    ~project_left ~project_right ~init ~merge seq1 seq2 : _ t =
  let module Tbl = Hashtbl.Make (Key) in
  let tbl_left = Tbl.create 16 in
  let get_l tbl k = try Tbl.find tbl k with Not_found -> [] in

  (* index all of [seq1] by key *)
  iter
    (fun x ->
      let key = project_left x in
      Tbl.replace tbl_left key (x :: get_l tbl_left key))
    seq1;

  let tbl = Tbl.create 16 in

  (* do product by iterating on [seq2] *)
  iter
    (fun y ->
      let key = project_right y in
      let xs = get_l tbl_left key in
      match xs with
      | [] -> ()
      | _ ->
        let acc = try Tbl.find tbl key with Not_found -> init in
        let acc = List.fold_left (fun acc x -> merge key x y acc) acc xs in
        Tbl.replace tbl key acc)
    seq2;

  Tbl.to_seq tbl |> map snd

module IO = struct
  let with_file_in ?(mode = 0o644) ?(flags = []) filename f =
    let ic = open_in_gen flags mode filename in
    try
      let x = f ic in
      close_in_noerr ic;
      x
    with e ->
      close_in_noerr ic;
      raise e

  let with_in ?mode ?flags filename f =
    with_file_in ?mode ?flags filename (fun ic ->
        f @@ of_gen
        @@ fun () -> try Some (input_char ic) with End_of_file -> None)

  let with_lines ?mode ?flags filename f =
    with_file_in ?mode ?flags filename (fun ic ->
        f @@ of_gen
        @@ fun () -> try Some (input_line ic) with End_of_file -> None)

  let with_file_out ?(mode = 0o644) ?(flags = [ Open_creat; Open_wronly ])
      filename f =
    let oc = open_out_gen flags mode filename in
    try
      let x = f oc in
      close_out oc;
      x
    with e ->
      close_out_noerr oc;
      raise e

  let write_str ?mode ?flags ?(sep = "") filename g =
    with_file_out ?mode ?flags filename (fun oc ->
        iteri
          (fun i s ->
            if i > 0 then output_string oc sep;
            output_string oc s)
          g)

  let write ?mode ?flags filename g =
    with_file_out ?mode ?flags filename (fun oc ->
        iter (fun c -> output_char oc c) g)

  let write_lines ?mode ?flags filename g =
    with_file_out ?mode ?flags filename (fun oc ->
        iter
          (fun s ->
            output_string oc s;
            output_char oc '\n')
          g)
end

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse (M : MONAD) = struct
  open M

  let map_m f l =
    let rec aux acc l =
      match l () with
      | Nil -> return (of_list (List.rev acc))
      | Cons (x, l') -> f x >>= fun x' -> aux (x' :: acc) l'
    in
    aux [] l

  let sequence_m l = map_m (fun x -> x) l

  let rec fold_m f acc l =
    match l () with
    | Nil -> return acc
    | Cons (x, l') -> f acc x >>= fun acc' -> fold_m f acc' l'
end

let pp ?(sep = ",") pp_item fmt l =
  let rec pp fmt l =
    match l () with
    | Nil -> ()
    | Cons (x, l') ->
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      pp_item fmt x;
      pp fmt l'
  in
  match l () with
  | Nil -> ()
  | Cons (x, l') ->
    pp_item fmt x;
    pp fmt l'

include Seq
