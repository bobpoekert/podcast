open OUnit2
open Cohttp

let with_test_warc_file thunk =
  let cwd = Sys.getcwd () in 
  let fname = Printf.sprintf "%s/rss.warc.gz" cwd in
  let fname = Re.replace_string (Re.Posix.compile_pat "_build/default/") ~by:"" fname in 
  let warc_file = Warc.load_file fname in 
  thunk warc_file; Warc.close_file warc_file

let assert_some opt =
  match opt with
  | None -> assert_failure "unexpected None"
  | Some(x) -> x

let string_stripper = Re.Pcre.regexp "^\\s*(.*?)\\s*$"

let strip s = Re.Group.get (Re.exec string_stripper s) 1

let test_first_entry _test_ctx =
  with_test_warc_file (fun inf ->
    let page = Warc.next_page inf in 
    let req = Warc.get_req page in 
    let rsp = Warc.get_rsp page in
    let req_headers = Warc.get_headers req in 
    let _rsp_headers = Warc.get_headers rsp in
    let url = Warc.get_url req_headers in 
    assert_equal ~msg:"url match fail" url "https://www.spreaker.com/show/1010000/episodes/feed"
  )

let splice_to a b =
  let length = min (String.length a) (String.length b) in 
  (
    (String.sub a 0 length),
    (String.sub b 0 length)
  )

let test_hashes _test_ctx =
  with_test_warc_file (fun inf ->
    Warc.iter_pages inf (fun page ->
      let rsp = Warc.get_rsp page in 
      let headers = Warc.get_headers rsp in
      let sha_string = strip (Warc.get_header headers "WARC-Block-Digest") in 
      let body = Warc.get_body rsp in 
      let header_length = int_of_string (Warc.get_header headers "Content-Length") in
      let body_sha_string = 
        body
        |> Sha1.string
        |> Sha1.to_bin
        |> Base32.base32_encode
        |> Printf.sprintf "sha1:%s" in
      let sha_string, body_sha_string = splice_to sha_string body_sha_string in
      assert_equal ~msg:"length not equal" (String.length body) header_length;
      assert_equal ~msg:"sha length not equal" (String.length body_sha_string) (String.length sha_string);
      assert_equal ~msg:"sha not equal" sha_string body_sha_string
    )
  )

let test_response_parse _test_ctx =
  with_test_warc_file (fun inf ->
    Warc.iter_pages inf (fun page ->
      let rsp = Warc.get_rsp page in 
      let body = Warc.get_body rsp in 
      let hrsp, _hbody = Warc.parse_response_body body in 
      let response_code = Code.code_of_status (Response.status hrsp) in 
      assert_bool "valid code" (response_code == 200 || response_code == 404);
    )
  )

let suite = 
  "suite">::: [
    "first_entry">:: test_first_entry;
    "hashes">:: test_hashes;
    "parse">:: test_response_parse
  ]

let () =
  run_test_tt_main suite
