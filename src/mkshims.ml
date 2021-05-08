
let code_before_407 =
{|
let rec seq_of_list_ l () =
  match l with
  | [] -> Seq.Nil
  | x :: tl -> Seq.Cons (x, seq_of_list_ tl)
module Tbl_make(H:Hashtbl.HashedType) = struct
  let seq_of_list = of_list
  include Hashtbl.Make(H)

  let to_seq tbl =
  let l = fold (fun k v l -> (k,v) :: l) tbl [] in
  seq_of_list_ l
end|}

let code_after_407 =
{|module Tbl_make = Hashtbl.Make
  |}

let () =
  let major, minor =
    Scanf.sscanf Sys.ocaml_version "%u.%u"
      (fun major minor -> major, minor)
  in
  let after_4_7 = (major, minor) >= (4, 7) in
  if after_4_7 then (
    print_string code_after_407
  ) else (
    print_string code_before_407
  )
