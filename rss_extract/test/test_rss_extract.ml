open OUnit2
open Lib_rss_extract

let test_warc_fname = 
  let cwd = Sys.getcwd () in 
  let fname = Printf.sprintf "%s/lib/warc/test/rss.warc.gz" cwd in
  Re.replace_string (Re.Posix.compile_pat "_build/default/test") ~by:"" fname

let with_test_warc_file thunk =
  let warc_file = Warc.load_file test_warc_fname in 
  thunk warc_file; Warc.close_file warc_file

let parse_xml s =
  try Xml.parse_string s with 
  | Xml.Error(msg, pos) -> (
    let l, r = Xml.abs_range pos in
    Printf.printf "%s -> %s" (Xml.error_msg msg) (String.sub s l r); raise Not_found)

let test_xml_parse _ =
  with_test_warc_file (fun inf ->
    Warc.iter_pages inf (fun page ->
      let rsp = Warc.get_rsp page in 
      let head, body = Warc.parse_response_body (Warc.get_body rsp) in
      let code = response_code head in 
      if code == 200 then let _ = parse_xml body in ();
    )
  )

let test_iter_xml_pages _ =
  let counter = ref 0 in 
  iter_xml_pages test_warc_fname (fun _req _header _hrsp _xml ->
    counter := !counter + 1
  );
  assert_bool "iter counter" (!counter > 10)

let list_exists f l =
  try
    let _ = List.find f l in true
  with | Not_found -> false

let test_channel_meta_text _ =
  let found_biz = ref false in
  iter_xml_pages test_warc_fname (fun _req _header _hrsp xml ->
    let text = channel_meta_text xml in 
    (if
      (list_exists (fun v -> (String.compare (unwrap_text_value v) "Technology") == 0) text)
     then
      found_biz := true);
    ()
  );
  assert_bool "tag exists" !found_biz

let rec _hash_equal h s =
  match s with 
  | Seq.Nil -> true
  | Seq.Cons((k, v), t) -> (
    let v2 = Hashtbl.find h k in 
    if v == v2 then 
      _hash_equal h (t ()) 
    else
      false
  )

let hash_equal a b =
  _hash_equal a ((Hashtbl.to_seq b) ())

let test_words_into_histogram _ =
  let test_text = "the the the it the is ?" in 
  let test_hist = Hashtbl.of_seq (List.to_seq [("the", 4); ("it", 1); ("is", 1); ("?", 1)]) in 
  let res_hist = Hashtbl.create 10 in
  words_into_histogram res_hist (Text test_text);
  assert_bool "word counts match" (hash_equal test_hist res_hist)

let suite = 
  "suite">::: [
    "xml_parse">:: test_xml_parse;
    "iter_xml_pages">:: test_iter_xml_pages;
    "channel_meta_text">:: test_channel_meta_text;
    "words_into_histogram">:: test_words_into_histogram
  ]

let () =
  run_test_tt_main suite