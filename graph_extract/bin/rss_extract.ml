open Cohttp
open Bigarray
open Utils

let load_cluster_ids fname = 
  let arr = load_array fname Int64 in 
  let len = Array1.dim arr in 
  let hashes = Array1.sub arr 0 (len / 2) in 
  let cluster_ids = Array1.sub arr (len / 2) (len / 2) in 
  (hashes, cluster_ids)


let url_cluster_id (hashes, clusters) url = 
  let k = url_hash url |> Int64.of_int in 
  let idx = binary_search hashes k in 
  Int64.to_int (Array1.get clusters idx)

let body_string inp =
  let _rsp, body = Warc.parse_response_body inp in 
  body

let body_xml inp =
  let _rsp, body = Warc.parse_response_body inp in
  Xml.parse_string body

let response_code rsp =
  Code.code_of_status (Response.status rsp)

let page_iter_callback thunk (page : Warc.warc_page) =
  let req = Warc.get_req page in 
  let rsp = Warc.get_rsp page in
  let body = Warc.get_body rsp in
  let hrsp, body = Warc.parse_response_body body in
  let code = response_code hrsp in
  let header = Warc.get_headers rsp in
  if code == 200 then
      try (
          let xml = Xml.parse_string body in
          thunk req header hrsp xml
      ) with _ -> ()

let load_file fname =
  (*TODO: SHELL INJECTION VULN! fix this to use create_process instead *)
  Unix.open_process_in (Printf.sprintf "gunzip -c %s" fname)

let close_file inf = close_in inf

let iter_xml_pages fname thunk = 
  let inf = load_file fname in
  Warc.iter_pages inf (page_iter_callback thunk);
  close_in inf

type text_value =
  | Text of string (* text blob, valid to tokenize *)
  | Tag of string (* opaque tag, should not be tokenized *)

let text_value_unwrap (v : text_value) =
  match v with
  | Text(v) -> v
  | Tag(v) -> v

let rec _xml_text res node =
  match node with
  | Xml.Element(_tag, _attrs, children) -> List.fold_left _xml_text res children
  | Xml.PCData(v) -> (Text(v) :: res)

let xml_text node = _xml_text [] node


let whitespace_re = Re.Pcre.regexp "\\s+"

let clean_text t = Re.replace_string ~all:true whitespace_re ~by:" " t

let clean_tag t = Re.replace_string ~all:true whitespace_re ~by:"_" t

let clean_xml_text (inp : text_value) = 
  match inp with
  | Tag(v) -> clean_tag v
  | Text(v) -> clean_text v

let unwrap_text_value (inp : text_value) = 
  match inp with
  | Tag(v) -> v
  | Text(v) -> v

let rec _channel_meta_text res xml =
  match xml with
  | Xml.PCData(_) -> res
  | Xml.Element(tag, _attrs, children) ->
    match tag with
    | "title" | "description" | "itunes:title" | "itunes:description" | "itunes:subtitle"
    | "itunes:summary" | "googleplay:description" ->
      (List.append (xml_text xml) res)
    | "itunes:category" | "googleplay:category" ->
      List.fold_left _channel_meta_text (Tag(Xml.attrib xml "text") :: res) children
    | _ -> List.fold_left _channel_meta_text res children

let channel_meta_text xml = _channel_meta_text [] xml


let () = 
  let outfname = Array.get Sys.argv 1 in 
  let fnames = Array.sub Sys.argv 2 ((Array.length Sys.argv) - 2) in 
  parparts fnames (fun part_idx fnames -> 
    with_out (Printf.sprintf "%s.%d" outfname part_idx) (fun fd -> 
      Array.iter (fun fname -> 
        iter_xml_pages fname (fun _req headers _head xml -> 
          let text = channel_meta_text xml |> List.map clean_xml_text |> String.concat " " in 
          let url = Warc.get_url headers in 
          let id = url_hash (clean_url url) in 
          output_string fd (string_of_int id);
          output_string fd "\t";
          output_string fd text;
          output_string fd "\n";
        );
      ) fnames;
    );
  );
