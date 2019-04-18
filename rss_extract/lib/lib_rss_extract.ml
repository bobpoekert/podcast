open Cohttp


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
  let _ = print_endline (Warc.get_url header) in
  if code == 200 then
    try
      let xml = Xml.parse_string body in
      thunk req header hrsp xml
    with | Xml.Error(_) | Dtd.Parse_error(_) | Xml.File_not_found(_) -> ()

let load_file fname =
  (*TODO: SHELL INJECTION VULN! fix this to use create_process instead *)
  Unix.open_process_in (Printf.sprintf "gunzip -c %s" fname)

let close_file inf = close_in inf

let iter_xml_pages fname thunk = 
  let inf = load_file fname in
  Warc.iter_pages inf (page_iter_callback thunk);
  Warc.close_file inf

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

let splitter_re = Re.Pcre.regexp "[^a-zA-Z0-9]+"

let rec _clean_tokens tokens res =
  match tokens with
  | [] -> res
  | h :: t ->
    match h with
    | `Text(v) -> _clean_tokens t (v :: res)
    | `Delim(_) -> _clean_tokens t res

let clean_tokens tokens = _clean_tokens tokens []

let tokenize_text (inp : text_value) = 
  match inp with
  | Tag(v) -> [v]
  | Text(v) -> clean_tokens (Re.split_full splitter_re v)

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
    | "item" -> res (* skip entries, we only want channel meta *)
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

let hist_update h k =
  let prev =
    try Hashtbl.find h k
    with Not_found -> 0 in
  Hashtbl.replace h k (prev + 1)

let into_histogram h items =
  List.iter (hist_update h) items

let words_into_histogram h text =
  into_histogram h (tokenize_text text)  

let item_guid item_xml =
  match first_tag_name "guid" item_xml with
  | Some(tag) -> Some(String.concat "" (List.map text_value_unwrap (xml_text tag)))
  | None ->
    match first_tag_name "enclosure" item_xml with
    | Some(tag) -> Some(Xml.attrib tag "url")
    | None -> None

let iter_word_histograms fname thunk =
  iter_xml_pages fname (fun (req : Warc.warc_entry) (headers : Warc.header) _head xml ->
    let meta_text = channel_meta_text xml in 
    let meta_hist = Hashtbl.create 4096 in
    List.iter (words_into_histogram meta_hist) meta_text;
    iter_item xml (fun item_xml ->
      let item_hist = Hashtbl.copy meta_hist in
      List.iter
        (words_into_histogram item_hist)
        (channel_meta_text item_xml);
      (thunk req headers item_xml item_hist)
    );
  )

let _absolute_error_folder topic topic_total hist_total k v res =
  (* hist is smaller than topic so start at topic_total and subtract *)
  let topic_v = float_of_int (Histogram.get topic k) in 
  let topic_normed = topic_v /. topic_total in 
  let v_normed = (float_of_int v) /. hist_total in
  let normed_delta = abs_float (v_normed -. topic_normed) in 
  res -. (topic_v *. (1. -. normed_delta))

let absolute_error hist hist_total topic =
  let topic_total = float_of_int (Histogram.sum topic) in
  let score_not_normed = Hashtbl.fold (_absolute_error_folder topic topic_total hist_total) hist topic_total in 
  score_not_normed /. topic_total

let hist_sum hist =
  Hashtbl.fold (fun _k v res -> res + v) hist 0

let iter_topic_vectors topics fname thunk =
  iter_word_histograms fname (fun req headers item_xml item_hist ->
    let ht = hist_sum item_hist in 
    let errors = Array.map (absolute_error item_hist (float_of_int ht)) topics in 
    let error_sum = Array.fold_left (+.) 0.0 errors in 
    let proportions = Array.map (fun e -> e /. error_sum) errors in 
    thunk req headers item_xml proportions
  )

let process_file topics fname outs =
  iter_topic_vectors topics fname (fun req _headers item_xml proportions ->
    let rss_url = Warc.get_url (Warc.get_headers req) in 
    let guid = match item_guid item_xml with | Some(v) -> v | None -> "" in 
    let vec_string = String.concat "," (Array.to_list (Array.map string_of_float proportions)) in
    let res_string = Printf.sprintf "%s\t%s\t%s\n" rss_url guid vec_string in 
    Gzip.output_substring outs res_string 0 (String.length res_string)
  )

let range n = List.init n (fun x -> x)

let process_batch hists outfname part =
  let outf = open_out outfname in 
  let outs = Gzip.open_out_chan outf in 
  for inf_idx = 0 to (Array.length part) - 1 do 
    let inf = Array.get part inf_idx in 
    process_file hists inf outs
  done;
  Gzip.close_out outs;
  close_out outf;
