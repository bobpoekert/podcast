open OUnit2

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

let base32_charset = Array.init 32 (fun x -> (Char.code (String.get "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" x)))

let padded inp size =
  let inlen = String.length inp in
  let res = Bytes.create size in
  let inp = Bytes.of_string inp in
  Bytes.blit inp 0 res 0 inlen;
  Bytes.fill res 0 inlen '\x00';
  Bytes.to_string res

let rec _unpack_int inp off len res =
  if off < len then
    _unpack_int inp (off + 1) len ((res lsl 8) lor (Char.code (String.get inp off)))
  else
    res

let rec _base32_encode inp res off limit =
  if off < limit then
    let c = _unpack_int inp 0 5 0 in 
    let enc_byte =
      (Array.get base32_charset (c lsr 30)) +
      (Array.get base32_charset ((c lsr 20) land 0x3ff)) +
      (Array.get base32_charset ((c lsr 10) land 0x3ff)) +
      (Array.get base32_charset (c land 0x3ff)) in 
    Bytes.set res off (Char.chr enc_byte);
    _base32_encode inp res (off + 5) limit

let base32_encode inp =
  let inlen = String.length inp in 
  let leftover = inlen mod 5 in 
  let inp = if leftover > 0 then padded inp (inlen + 5 - leftover) else inp in 
  let reslen = (String.length inp) / 5 in
  let res = Bytes.create reslen in 
    _base32_encode inp res 0 (String.length inp);
    (match leftover with
    | 1 -> Bytes.fill res (reslen - 6) 6 '='
    | 2 -> Bytes.fill res (reslen - 4) 4 '='
    | 3 -> Bytes.fill res (reslen - 3) 3 '='
    | 4 -> Bytes.fill res (reslen - 1) 1 '='
    | _ -> ());
    Bytes.to_string res

let test_first_entry _test_ctx =
  with_test_warc_file (fun inf ->
    let page = Warc.next_page inf in 
    let req = Warc.get_req page in 
    let rsp = Warc.get_rsp page in
    let req_headers = Warc.get_headers req in 
    let _rsp_headers = Warc.get_headers rsp in
    let url = Warc.get_url req_headers in 
    assert_equal url "https://www.spreaker.com/show/1010000/episodes/feed"
  )

let test_hashes _test_ctx =
  with_test_warc_file (fun inf ->
    Warc.iter_pages inf (fun page ->
      let rsp = Warc.get_rsp page in 
      let sha_string = Warc.get_header (Warc.get_headers rsp) "WARC-Payload-Digest" in 
      let body = Warc.get_body rsp in 
      let body_sha_string = 
        body
        |> Sha1.string
        |> Sha1.to_bin
        |> base32_encode
        |> Printf.sprintf "sha1:%s" in
      Printf.printf "%s %s\n" sha_string body_sha_string;
      assert_equal sha_string body_sha_string
    )
  )

let suite = 
  "suite">::: [
    "first_entry">:: test_first_entry;
    "hashes">:: test_hashes
  ]

let () =
  run_test_tt_main suite
