type header_row = (string * string)
type header = header_row list
type warc_entry = (header * string)
type warc_page = (warc_entry * warc_entry) (* request, response *)

let rec _readline res ins =
  let c = Gzip.input_char ins in
  match c with
  | '\n' -> res
  | c -> Buffer.add_char res c; _readline res ins

let readline ins =
  let buf = _readline (Buffer.create 4096) ins in 
  Bytes.to_string (Buffer.to_bytes buf)

let string_stripper = Re.Pcre.regexp "^\\s*(.*?)\\s*$"

let strip s = Re.Group.get (Re.exec string_stripper s) 1

let rec _parse_headers fields ins =
  let line = readline ins in 
  match line with
  | "\r" -> fields
  | "WARC/1.0\r" -> _parse_headers fields ins
  | _ ->
    let strlen = String.length line in
    let colon_idx = String.index line ':' in
    let k = String.sub line 0 colon_idx in 
    let v = String.sub line (colon_idx + 2) (strlen - colon_idx - 2) in 
    _parse_headers ((k, (strip v)) :: fields) ins

let parse_headers ins = _parse_headers [] ins

let get_header headers k =
  let _, v = List.find (fun (kk, _) -> String.equal kk k) headers in v

let rec _read_gz_bytes start target_len inf res =
  let read_len = Gzip.input inf res start target_len in
  if read_len == 0 then
    raise End_of_file 
  else if read_len < target_len then
    _read_gz_bytes (start + read_len) (target_len - read_len) inf res

let read_gz_bytes n_bytes inf = 
  let res = Bytes.create n_bytes in 
  _read_gz_bytes 0 n_bytes inf res;
  Bytes.to_string res

let next_entry (ins : Gzip.in_channel) : warc_entry =
    let header_fields = parse_headers ins in
    let length_field = get_header header_fields "Content-Length" in
    let length_int = int_of_string (strip length_field) in
    let body = read_gz_bytes length_int ins in
    let _ = readline ins in (* throw away blank line *)
    let _ = readline ins in (* throw away blank line *)
    (header_fields, body)

exception Incomplete_request_error

let rec _next_page (req : warc_entry option) (inp : Gzip.in_channel) : (warc_entry * warc_entry) =
  let entry = next_entry inp in
  let headers, _ = entry in
  let warc_type = strip (get_header headers "WARC-Type") in
  match warc_type with
  | "request" -> (_next_page (Some entry) inp)
  | "response" -> (match req with
    | Some(r) -> (r, entry)
    | None -> raise Incomplete_request_error)
  | _ -> (_next_page req inp)

let next_page inp = _next_page None inp

let rec _iter_pages inp thunk =
  try
    let page = next_page inp in
      thunk page; _iter_pages inp thunk
  with
  | End_of_file -> ()
  | Incomplete_request_error -> _iter_pages inp thunk

let iter_pages inp thunk = _iter_pages inp thunk

let get_url headers = get_header headers "WARC-Target-URI"

let load_file fname = Gzip.open_in fname
let close_file inf = Gzip.close_in inf

let get_req (x : warc_page) =
  let req, _ = x in req

let get_rsp (x : warc_page) =
  let _, rsp = x in rsp

let get_headers (x : warc_entry) =
  let h, _v = x in h

let get_body (x : warc_entry) =
  let _h, v = x in v