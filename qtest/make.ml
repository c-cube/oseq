
let () =
  if Sys.command "which qtest > /dev/null" <> 0 then (
    (* create empty file *)
  ) else (
    let files = "../src/OSeq.ml ../src/OSeq.mli" in
    let cmd =
      Printf.sprintf "qtest extract %s 2>/dev/null"
        files
    in
    exit (Sys.command cmd)
  )
