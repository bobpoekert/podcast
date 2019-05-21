type header_row = (string * string)
type header = header_row list
type warc_entry = (header * string)
type warc_page = (warc_entry * warc_entry) (* request, response *)

exception Incomplete_request
exception Invalid_request of string

open Cohttp

module String_io = Cohttp__String_io
module StringResponse = Response.Make(String_io.M)
module StringRequest = Request.Make(String_io.M)

let allowed_body response = (* rfc7230#section-5.7.1 *)
    match Response.status response with
    | #Code.informational_status | `No_content | `Not_modified -> false
    | #Code.status_code -> true

let has_body response =
  if allowed_body response
  then Transfer.has_body (Response.encoding response)
  else `No

let rec _consume_chunks rdr res tot_size =
  match StringResponse.read_body_chunk rdr with
  | Transfer.Final_chunk(v) -> ((v :: res), (tot_size + String.length v))
  | Transfer.Done -> (res, tot_size)
  | Transfer.Chunk(v) -> _consume_chunks rdr (v :: res) (tot_size + String.length v)

let rec _list_into_bytes res lst off =
  match lst with 
  | [] -> ()
  | s :: t -> (let slen = String.length s in 
    Bytes.blit_string s 0 res (off - slen) slen; 
    if off > slen then _list_into_bytes res t (off - slen)
  )

let consume_chunks rdr =
  let chunk_list, res_size = _consume_chunks rdr [] 0 in 
  match chunk_list with
  | v :: [] -> v 
  | [] -> "" 
  | chunk_list -> (
    let res = Bytes.create res_size in
    _list_into_bytes res chunk_list res_size;
    Bytes.to_string res
  )

let parse_response_body inp : (Response.t * string) =
  let inp = String_io.open_in inp in 
  let head = StringResponse.read inp in 
  match head with
  | `Eof -> raise Incomplete_request
  | `Invalid(v) -> raise (Invalid_request v)
  | `Ok(head) -> 
  (
    match has_body head with
    | `No -> (head, "")
    | `Unknown | `Yes -> (
      let reader = StringResponse.make_body_reader head inp in
      (head, consume_chunks reader)
    )
  )

let parse_request_body inp : Request.t =
  let inp = String_io.open_in inp in 
  let head = StringRequest.read inp in 
  match head with 
  | `Eof -> raise Incomplete_request 
  | `Invalid(v) -> raise (Invalid_request v) 
  | `Ok(head) -> head

let rec _parse_headers fields ins =
  let line = input_line ins in 
  match line with
  | "\r" -> fields
  | "WARC/1.0\r" -> _parse_headers fields ins
  | _ ->
    let strlen = String.length line in
    let colon_idx = String.index line ':' in
    let k = String.sub line 0 colon_idx in 
    let v = String.sub line (colon_idx + 2) (strlen - colon_idx - 3) in 
    _parse_headers ((k, v) :: fields) ins

let parse_headers ins = _parse_headers [] ins

let get_header headers k =
  let _, v = List.find (fun (kk, _) -> String.equal kk k) headers in v

let read_bytes n_bytes inf = 
  let res = Bytes.create n_bytes in 
  really_input inf res 0 n_bytes;
  Bytes.to_string res

let next_entry (ins : in_channel) : warc_entry =
    let header_fields = parse_headers ins in
    let length_field = get_header header_fields "Content-Length" in
    let length_int = int_of_string length_field in
    let body = read_bytes length_int ins in
    let _ = input_line ins in (* throw away blank line *)
    let _ = input_line ins in (* throw away blank line *)
    (header_fields, body)

let rec _next_page (req : warc_entry option) (inp : in_channel) : (warc_entry * warc_entry) =
  let entry = next_entry inp in
  let headers, _ = entry in
  let warc_type = get_header headers "WARC-Type" in
  match warc_type with
  | "request" -> (_next_page (Some entry) inp)
  | "response" -> (match req with
    | Some(r) -> (r, entry)
    | None -> raise Incomplete_request)
  | _ -> (_next_page req inp)

let next_page inp = _next_page None inp

let rec _iter_pages inp thunk =
  try
    while true do
      let page = next_page inp in
      thunk page;
    done
  with
  | End_of_file -> ()
  | Incomplete_request -> _iter_pages inp thunk

let iter_pages inp thunk = _iter_pages inp thunk

let get_url headers = get_header headers "WARC-Target-URI"

let get_req (x : warc_page) =
  let req, _ = x in req

let get_rsp (x : warc_page) =
  let _, rsp = x in rsp

let get_headers (x : warc_entry) =
  let h, _v = x in h

let get_body (x : warc_entry) =
  let _h, v = x in v