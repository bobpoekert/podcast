open Cohttp
open Tokens

let body_string inp =
  let _rsp, body = Warc.parse_response_body inp in 
  body

let body_xml inp =
  let _rsp, body = Warc.parse_response_body inp in
  Xml.parse_string body

let response_code rsp =
  Code.code_of_status (Response.status rsp)

let rec _xml_text res node =
  match node with
  | Xml.Element(_tag, _attrs, children) -> List.fold_left _xml_text res children
  | Xml.PCData(v) -> (Text(v) :: res)
let xml_text node = _xml_text [] node

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

let iter_xml_pages fname thunk = 
  let inf = load_file fname in
  Warc.iter_pages inf (page_iter_callback thunk);
  close_in inf

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

let rec _iter_tag_name tag_name thunk xml =
  match xml with
  | Xml.PCData(_) -> ()
  | Xml.Element(tag, _attrs, children) ->
    if (String.compare tag_name tag) == 0 then thunk xml;
    List.iter (_iter_tag_name tag_name thunk) children

let iter_tag_name xml tag_name thunk = _iter_tag_name tag_name thunk xml
let iter_item xml thunk = iter_tag_name xml "item" thunk

let rec _first_tag_name tag_name xmls = 
  match xmls with
  | [] -> None
  | xml :: rest ->
    match xml with
    | Xml.PCData(_) -> _first_tag_name tag_name rest
    | Xml.Element(tag, _attrs, children) ->
      if (String.compare tag tag_name) == 0 then Some(xml) else 
      _first_tag_name tag_name (List.append rest children)

let first_tag_name tag_name xml = _first_tag_name tag_name [xml]


let item_guid item_xml =
  match first_tag_name "guid" item_xml with
  | Some(tag) -> Some(String.concat "" (List.map text_value_unwrap (xml_text tag)))
  | None ->
    match first_tag_name "enclosure" item_xml with
    | Some(tag) -> Some(Xml.attrib tag "url")
    | None -> None
