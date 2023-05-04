## 0.5

- use ocamlformat
- add let-operators
- require OCaml 4.08
- breaking: remove most labels, ensure compatibility with extended Seq

## 0.4.1

- fix error in tests related to `-nolabels`

## 0.4

- use `include Seq` so we're forward compatible with OCaml
- add relational operators: group_by, group_by_fold, join_by, join_by_fold
- add `{flat_map,app}_interleave`, `to_iter`, `to_gen`
- add head_exn/tail_exn

- fix: make `product` more fair by using interleave
- fix: handle exceptions in OSeq.memoize
