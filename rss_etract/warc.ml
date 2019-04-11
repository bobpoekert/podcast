type header_row = (string * string)
type header = header_row list
type warc_entry = (header * bytes) option
type warc_page = (warc_entry * warc_entry) (* request, response *)

let rec _readline res ins =
  let c = Gzip.input_char ins in
  match c with
  | '\n' -> res
  | c -> Buffer.add_char res c; _readline res ins

let readline ins =
  let buf = _readline (Buffer.create 4096) ins in 
  Bytes.to_string (Buffer.to_bytes buf)

let rec _parse_headers fields ins =
  let line = readline ins in 
  match line with
  | "\r" -> fields
  | "WARC/1.0\r" -> _parse_headers fields ins
  | _ ->
    let strlen = String.length line in
    let colon_idx = String.index line ':' in
    let k = String.sub line 0 colon_idx in 
    let v = String.sub line (colon_idx + 1) (strlen - 1) in 
    _parse_headers ((k, v) :: fields) ins

let parse_headers ins = _parse_headers [] ins

let get_header headers k =
  let _, v = List.find (fun (kk, _) -> kk == k) headers in v

let next_entry (ins : Gzip.in_channel) : warc_entry =
    let header_fields = parse_headers ins in
    let length_field = get_header header_fields "Content-Length" in
    let length_int = int_of_string length_field in 
    let body = Bytes.create length_int in
    let input_status = Gzip.input ins body 0 length_int in 
    if input_status == 0 then None else Some(header_fields, body)

let rec _next_page req inp =
  let entry = next_entry inp in
  match entry with
  | None -> _next_page req inp 
  | Some(headers, _) ->
    let warc_type = get_header headers "WARC-Type" in 
    match warc_type with
    | "request" -> _next_page entry inp 
    | "response" -> Some(req, entry)
    | _ -> _next_page req inp 

let next_page inp = _next_page None inp

let rec _iter_pages inp thunk =
  try 
    match next_page inp with
    | None -> _iter_pages inp thunk
    | Some(page) -> thunk page; _iter_pages inp thunk
  with | End_of_file -> ()

let iter_pages inp thunk = _iter_pages inp thunk

let get_url headers = get_header headers "WARC-Target-URI"

let load_file fname = Gzip.open_in fname
let close_file inf = Gzip.close_in inf

let get_req (x : warc_page) =
  let req, _ = x in req

let get_rsp (x : warc_page) =
  let _, rsp = x in rsp

exception Not_entry

let get_header (x : warc_entry) =
  match x with
  | Some(h, _v) -> h
  | None -> raise Not_entry

let get_body (x : warc_entry) =
  match x with
  | Some(_h, v) -> Bytes.to_string v
  | None -> raise Not_entry