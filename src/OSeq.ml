
(** {1 OSeq: Functional Iterators} *)


(*$inject

  let pint i = string_of_int i
  let pilist l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp Format.pp_print_int) (Gen.of_list l);
    Buffer.contents b
  let pi2list l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp (fun fmt (a,b) -> Format.fprintf fmt "%d,%d" a b))
      (Gen.of_list l);
    Buffer.contents b
  let pstrlist l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp Format.pp_print_string) (Gen.of_list l);
    Buffer.contents b
*)

type 'a t = unit -> 'a node

and 'a node =
  | Nil
  | Cons of 'a * 'a t

let rec (--) i j () =
  if i>j then Nil
  else Cons (i, i+1 -- j)

let rec map f l () =
  match l () with
    | Nil -> Nil
    | Cons (x,tail) -> Cons (f x, map f tail)

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

let rec flat_map f l () =
  match l() with
    | Nil -> Nil
    | Cons (x,tl) ->
      let res = append (f x) (flat_map f tl) in
      res ()

let rec fold f acc l = match l() with
  | Nil -> acc
  | Cons (x,tl) -> fold f (f acc x) tl
