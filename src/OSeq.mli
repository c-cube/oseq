
(** {1 OSeq: Functional Iterators} *)

type 'a t = unit -> 'a node

and 'a node = 'a Seq.node =
  | Nil
  | Cons of 'a * 'a t

type 'a seq = 'a t (* alias *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a printer = Format.formatter -> 'a -> unit

val empty : 'a t
(** Empty iterator, with no elements *)

val return : 'a -> 'a t
(** One-element iterator *)

val cons : 'a -> 'a t -> 'a t

val repeat : 'a -> 'a t
(** Repeat same element endlessly *)

val cycle : 'a t -> 'a t
(** Cycle through the iterator infinitely. The iterator shouldn't be empty.
    {[# OSeq.(cycle (1--3) |> take 10 |> to_list);;
      - : int list = [1; 2; 3; 1; 2; 3; 1; 2; 3; 1]
    ]}
*)

val iterate : 'a -> ('a -> 'a) -> 'a t
(** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]].

    {[# OSeq.(iterate 0 succ |> take 10 |> to_list);;
      - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]
    ]}
*)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** Dual of {!fold}, with a deconstructing operation. It keeps on
    unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
    until [None] is returned.

    {[# OSeq.(unfold (fun x -> if x<5 then Some (string_of_int x, x+1) else None) 0 |> to_list);;
      - : string list = ["0"; "1"; "2"; "3"; "4"]
    ]}
*)

val repeatedly : (unit -> 'a) -> 'a t
(** Call the same function an infinite number of times (useful for instance
    if the function is a random iterator). *)

val init : ?n:int -> (int -> 'a) -> 'a t
(** Calls the function, starting from 0, on increasing indices.
    If [n] is provided and is a positive int, iteration will
    stop at the limit (excluded).
    For instance [init ~n:4 (fun x->x)] will yield 0, 1, 2, and 3. *)

(** {2 Basic combinators} *)

val is_empty : _ t -> bool
(** Check whether the iterator is empty. Pops an element, if any *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Fold on the iterator, tail-recursively. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** Alias to {!fold} *)

val reduce : ('a -> 'a -> 'a) -> 'a t -> 'a
(** Fold on non-empty iterators.
    @raise Invalid_argument on an empty iterator *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** Like {!fold}, but keeping successive values of the accumulator.

    {[
      # OSeq.(scan (+) 0 (1--5) |> to_list);;
      - : int list = [0; 1; 3; 6; 10; 15]
    ]}
*)

val unfold_scan : ('b -> 'a -> 'b * 'c) -> 'b -> 'a t -> 'c t
(** A mix of {!unfold} and {!scan}. The current state is combined with
    the current element to produce a new state, and an output value
    of type 'c. *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on the iterator . *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Iterate on elements with their index in the iterator, from 0. *)

val length : _ t -> int
(** Length of an iterator (linear time). *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Lazy map. No iteration is performed now, the function will be called
    when the result is traversed. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Lazy map with indexing starting from 0. No iteration is performed now,
    the function will be called when the result is traversed. *)

val app : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative *)

val fold_map : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** Lazy fold and map. No iteration is performed now, the function will be
    called when the result is traversed. The result is
    an iterator over the successive states of the fold.
    The final accumulator is discarded.
    Unlike {!scan}, fold_map does not return the first accumulator.
*)

val append : 'a t -> 'a t -> 'a t
(** Append the two iterators; the result contains the elements of the first,
    then the elements of the second iterator. *)

val flatten : 'a t t -> 'a t
(** Flatten the iterator of iterators *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Monadic bind; each element is transformed to a sub-iterator
    which is then iterated on, before the next element is processed,
    and so on. *)

val mem : eq:('a -> 'a -> bool) -> 'a -> 'a t -> bool
(** Is the given element, member of the iterator? *)

val take : int -> 'a t -> 'a t
(** Take at most n elements *)

val drop : int -> 'a t -> 'a t
(** Drop n elements *)

val nth : int -> 'a t -> 'a
(** n-th element, or Not_found
    @raise Not_found if the iterator contains less than [n] arguments *)

val take_nth : int -> 'a t -> 'a t
(** [take_nth n g] returns every element of [g] whose index
    is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
    will return [[1;3;5;7;9]] *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Filter out elements that do not satisfy the predicate.  *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** Take elements while they satisfy the predicate. The initial iterator
    itself is not to be used anymore after this. *)

val fold_while : ('a -> 'b -> 'a * [`Stop | `Continue]) -> 'a -> 'b t -> 'a
(** Fold elements until (['a, `Stop]) is indicated by the accumulator. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** Drop elements while they satisfy the predicate. The initial iterator
    itself should not be used anymore, only the result of [drop_while]. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Maps some elements to 'b, drop the other ones *)

val zip_index : 'a t -> (int * 'a) t
(** Zip elements with their index in the iterator *)

val unzip : ('a * 'b) t -> 'a t * 'b t
(** Unzip into two iterators, splitting each pair *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p l] returns the elements that satisfy [p],
    and the elements that do not satisfy [p] *)

val for_all : ('a -> bool) -> 'a t -> bool
(** Is the predicate true for all elements? *)

val exists : ('a -> bool) -> 'a t -> bool
(** Is the predicate true for at least one element? *)

val min : lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Minimum element, according to the given comparison function.
    @raise Invalid_argument if the iterator is empty *)

val max : lt:('a -> 'a -> bool) -> 'a t -> 'a
(** Maximum element, see {!min}
    @raise Invalid_argument if the iterator is empty *)

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Equality of iterators. *)

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Lexicographic comparison of iterators. If a iterator is a prefix
    of the other one, it is considered smaller. *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p e] returns the first element of [e] to satisfy [p],
    or None. *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f e] returns the result of [f] on the first element of [e]
    for which it returns [Some _], or [None] otherwise.
    @since NEXT_RELEASE *)

val sum : int t -> int
(** Sum of all elements *)

(** {2 Multiple iterators} *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Map on the two iterators. Stops once one of them is exhausted.*)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** Iterate on the two iterators. Stops once one of them is exhausted.*)

val fold2 : ('acc -> 'a -> 'b -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
(** Fold the common prefix of the two iterators *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Succeeds if all pairs of elements satisfy the predicate.
    Ignores elements of an iterator if the other runs dry. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Succeeds if some pair of elements satisfy the predicate.
    Ignores elements of an iterator if the other runs dry. *)

val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Combine common part of the gens (stops when one is exhausted) *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** Zip together the common part of the gens *)

(** {2 Complex combinators} *)

val merge : 'a t t -> 'a t
(** Pick elements fairly in each sub-iterator. The merge of gens
    [e1, e2, ... ] picks elements in [e1], [e2],
    in [e3], [e1], [e2] .... Once an iterator is empty, it is skipped;
    when they are all empty, and none remains in the input,
    their merge is also empty.
    For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. *)

val intersection : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
(** Intersection of two sorted iterators. Only elements that occur in both
    inputs appear in the output *)

val sorted_merge : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
(** Merge two sorted iterators into a sorted iterator *)

val round_robin : ?n:int -> 'a t -> 'a t list
(** Split the iterator into [n] iterators in a fair way. Elements with
    [index = k mod n] with go to the k-th iterator. [n] default value
    is 2. *)

val interleave : 'a t -> 'a t -> 'a t
(** [interleave a b] yields an element of [a], then an element of [b],
    and so on. When a iterator is exhausted, this behaves like the
    other iterator. *)

val intersperse : 'a -> 'a t -> 'a t
(** Put the separator element between all elements of the given iterator *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** Cartesian product, in no predictable order. Works even if some of the
    arguments are infinite. *)

val product3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** Cartesian product of three iterators, see product.
    @since 0.2 *)

val product4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** Cartesian product of four iterators, see product.
    @since 0.2 *)

val product5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('a * 'b * 'c * 'd * 'e) t
(** Cartesian product of five iterators, see product.
    @since 0.2 *)

val product6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t
   ->  ('a * 'b * 'c * 'd * 'e * 'f) t
(** Cartesian product of six iterators, see product.
    @since 0.2 *)

val product7 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> 'g t
   ->  ('a * 'b * 'c * 'd * 'e * 'f * 'g) t
(** Cartesian product of seven iterators, see product.
    @since 0.2 *)

val cartesian_product : 'a t t -> 'a list t
(** Produce the cartesian product of this list of lists,
    by returning all the ways of picking one element per sublist.
    {b NOTE} the order of the returned list is unspecified.
    For example:
    {[
      # cartesian_product [[1;2];[3];[4;5;6]] |> sort =
      [[1;3;4];[1;3;5];[1;3;6];[2;3;4];[2;3;5];[2;3;6]];;
      # cartesian_product [[1;2];[];[4;5;6]] = [];;
      # cartesian_product [[1;2];[3];[4];[5];[6]] |> sort =
      [[1;3;4;5;6];[2;3;4;5;6]];;
    ]}
    invariant: [cartesian_product l = map_product_l id l].
    @since 0.2 *)

val map_product_l : ('a -> 'b t) -> 'a t -> 'b list t
(** [map_product_l f l] maps each element of [l] to a list of
    objects of type ['b] using [f].
    We obtain [[l1;l2;...;ln]] where [length l=n] and [li : 'b list].
    Then, it returns all the ways of picking exactly one element per [li].
    @since 0.2 *)

val group : eq:('a -> 'a -> bool) -> 'a t -> 'a t t
(** Group equal consecutive elements together. *)

val uniq : eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** Remove consecutive duplicate elements. Basically this is
    like [fun e -> map List.hd (group e)]. *)

val sort : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort according to the given comparison function. The iterator must be finite. *)

val sort_uniq : cmp:('a -> 'a -> int) -> 'a t -> 'a t
(** Sort and remove duplicates. The iterator must be finite. *)

val chunks : int -> 'a t -> 'a array t
(** [chunks n e] returns a iterator of arrays of length [n], composed
    of successive elements of [e]. The last array may be smaller
    than [n] *)

val permutations : 'a list -> 'a list t
(** Permutations of the list. *)

val combinations : int -> 'a t -> 'a list t
(** Combinations of given length. The ordering of the elements within
    each combination is unspecified.
    Example (ignoring ordering):
      [combinations 2 (1--3) |> to_list = [[1;2]; [1;3]; [2;3]]] *)

val power_set : 'a t -> 'a list t
(** All subsets of the iterator (in no particular order). The ordering of
    the elements within each subset is unspecified. *)

(** {2 Basic conversion functions} *)

val of_list : 'a list -> 'a t
(** Enumerate elements of the list *)

val to_list : 'a t -> 'a list
(** non tail-call trasnformation to list, in the same order *)

val to_rev_list : 'a t -> 'a list
(** Tail call conversion to list, in reverse order (more efficient) *)

val to_array : 'a t -> 'a array
(** Convert the iterator to an array (not very efficient) *)

val of_array : ?start:int -> ?len:int -> 'a array -> 'a t
(** Iterate on (a slice of) the given array *)

val of_gen : 'a gen -> 'a t
(** Build a functional iterator from a mutable, imperative generator.
    The result is properly memoized and can be iterated on several times,
    as a normal functional value. *)

val of_gen_transient : 'a gen -> 'a t
(** Build a functional iterator from a mutable, imperative generator.
    Note that the resulting iterator is not going to be really functional
    because the underlying generator can be consumed only once.
    Use {!memoize} to recover the proper semantics, or use {!of_gen}
    directly. *)

val of_string : ?start:int -> ?len:int -> string -> char t
(** Iterate on bytes of the string *)

val to_string : char t -> string
(** Convert into a string *)

val to_buffer : Buffer.t -> char t -> unit
(** Traverse the iterator and writes its content to the buffer *)

val lines : char t -> string t
(** Group together chars belonging to the same line *)

val unlines : string t -> char t
(** Explode lines into their chars, adding a ['\n'] after each one *)

module Infix : sig
  val (--) : int -> int -> int t

  val (--^) : int -> int -> int t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Monadic bind operator *)

  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix map operator *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix map operator *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

include module type of Infix

val pp : ?sep:string -> 'a printer -> 'a t printer
(** Pretty print the content of the iterator on a formatter. *)

val memoize : 'a t -> 'a t
(** Store content of the transient iterator in memory, to be able to iterate
    on it several times later. *)

(** {2 Easy interface to Produce Iterators} *)

(** This interface is designed to make it easy to build complex streams of
    values in a way that resembles Python's generators (using "yield").

    {[
      let naturals : int OSeq.t = OSeq.Generator.(
          let rec aux n = yield n >>= fun () -> aux (n+1) in
          run (aux 0)
        )
    ]}

    {[
      type 'a tree = E | N of 'a tree * 'a * 'a tree

      let traverse (t:'a tree) : 'a OSeq.t =
        let open OSeq.Generator in
        let rec trav = function
          | E -> empty
          | N (l,v,r) -> trav l >>= fun () -> yield v >>= fun () -> trav r
        in
        run (trav t)
    ]}

*)
module Generator : sig
  type 'a t
  (** Type for writing generators (of type ['a OSeq.Generator.t])
      that can be used to construct an iterator of type ['a OSeq.t] *)

  val empty : 'a t
  (** Empty generator, yields no value *)

  val yield : 'a -> 'a t
  (** Yield one value *)

  val (>>=) : 'a t -> (unit -> 'a t) -> 'a t
  (** [gen1 >>= fun () -> gen2] first yields all values from [gen1],
      and then all values from [gen2] *)

  val delay : (unit -> 'a t) -> 'a t
  (** Delayed generator, will evaluate the function when needed *)

  val run : 'a t -> 'a seq
  (** Iterator over the values yielded by the generator *)
end

(** {2 Basic IO} *)
module IO : sig
  val with_in : ?mode:int -> ?flags:open_flag list ->
    string ->
    (char t -> 'a) -> 'a
  (** [with_in filename f] opens [filename] and calls [f g],
      where [g] is a generator of characters from the file.
      The generator is only valid within
      the scope in which [f] is called. *)

  val with_lines : ?mode:int -> ?flags:open_flag list ->
    string -> (string t -> 'a) -> 'a
  (** [with_lines filename f] opens file [filename] and calls [f g],
      where [g] is a generator that iterates on the lines from the file.
      Do not use the generator outside of the scope of [f] *)

  val write_str : ?mode:int -> ?flags:open_flag list ->  ?sep:string ->
    string -> string t -> unit
  (** [write_to filename g] writes all strings from [g] into the given
      file. It takes care of opening and closing the file. Does not
      add [sep] after the last string.
      @param mode default [0o644]
      @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]].
      @param sep separator between each string (e.g. newline) *)

  val write : ?mode:int -> ?flags:open_flag list ->
    string -> char t -> unit
  (** Same as {!write_str} but with individual characters *)

  val write_lines : ?mode:int -> ?flags:open_flag list ->
    string -> string t -> unit
  (** [write_lines file g] is similar to [write_str file g ~sep:"\n"] but
      also adds ['\n'] at the end of the file *)

end

(** {2 Monadic Operations} *)
module type MONAD = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module Traverse(M : MONAD) : sig
  val sequence_m : 'a M.t t -> 'a t M.t

  val fold_m : ('b -> 'a -> 'b M.t) -> 'b -> 'a t -> 'b M.t

  val map_m : ('a -> 'b M.t) -> 'a t -> 'b t M.t
end
