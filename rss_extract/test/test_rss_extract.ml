open OUnit2
open Lib_rss_extract

let with_test_warc_file thunk =
  let cwd = Sys.getcwd () in 
  let fname = Printf.sprintf "%s/lib/warc/test/rss.warc.gz" cwd in
  let fname = Re.replace_string (Re.Posix.compile_pat "_build/default/test") ~by:"" fname in 
  let warc_file = Warc.load_file fname in 
  thunk warc_file; Warc.close_file warc_file

let ok_re = Re.Pcre.regexp "HTTP/1.1 200"

let is_ok rsp = Re.test (Re.exec ok_re rsp)

let parse_xml s =
  try Xml.parse_string s with 
  | Xml.Error(msg, pos) -> (
    let l, r = Xml.abs_range pos in
    Printf.printf "%s -> %s" (Xml.error_msg msg) (String.sub s l r); raise Not_found)

let test_xml_parse _ =
  with_test_warc_file (fun inf ->
    Warc.iter_pages inf (fun page ->
      let rsp = Warc.get_rsp page in 
      let body = Warc.get_body rsp in
      let _ = parse_xml (body_string body) in ()
    )
  )

let suite = 
  "suite">::: [
    "xml_parse">:: test_xml_parse
  ]

let () =
  run_test_tt_main suite