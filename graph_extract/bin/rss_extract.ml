open Cohttp
open Bigarray

let load_array fname dtype = 
  let size_bytes = (Unix.stat fname).st_size in 
  let item_size = Bigarray.kind_size_in_bytes dtype in 
  let n_items = size_bytes / item_size in 
  let fd = Unix.openfile fname [] 0o640 in 
  let arr = Unix.map_file fd dtype c_layout false [| n_items |] in
  array1_of_genarray arr

let load_cluster_ids fname = 
  let arr = load_array fname Int64 in 
  let len = Array1.dim arr in 
  let hashes = Array1.sub arr 0 (len / 2) in 
  let cluster_ids = Array1.sub arr (len / 2) (len / 2) in 
  (hashes, cluster_ids)

let rec _binary_search arr k l r =
  if l <= r then 
    let m = ((l + r) / 2) in 
    let v = Array1.get arr m in 
    if v < k then 
      _binary_search arr k (m + 1) r
    else if v > k then 
      _binary_search arr k l (m - 1)
    else m 
  else raise Not_found

let binary_search arr k = _binary_search arr k 0 ((Array1.dim arr) - 1)

let url_cluster_id (hashes, clusters) url = 
  let url = List.nth (String.split_on_char ':' url) 1 in 
  let url = Printf.sprintf ":%s" url in 
  let k = Murmur.murmur_hash url in 
  let k = Int64.of_int k in
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
  let prev = Hashtbl.find h k in
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

let rec _partition parts inp = 
  match inp with 
  | [] -> parts
  | _item :: inp -> (
    let part = List.hd parts in 
    let parts = List.tl parts in 
    _partition (List.append parts [part]) inp
  )

let partition n_parts inp = 
  let tot_len  = Array.length inp in 
  let part_size = tot_len / n_parts in 
  let res = Array.make n_parts inp in 
  for i = 0 to (n_parts - 1) do 
    Array.set res i (Array.sub inp (part_size * i) part_size)
  done;
  res

let spawn_worker combiner part pipe_out = 
  if Unix.fork () == 0 then (
    let res = combiner part in 
    let _ = print_endline "---" in 
    let chan_out = Unix.out_channel_of_descr pipe_out in 
    Marshal.to_channel chan_out res [];
    flush_all ();
    exit 0;
  )

let maprange f n =
  let first = f 0 in 
  let res = Array.make n first in 
  Array.set res 0 first;
  for i = 1 to (n - 1) do 
    Array.set res i (f i)
  done;
  res

let contains l v = List.exists (fun vv -> vv == v) l
let remove a b = List.filter (fun v -> not (contains b v)) a

let rec reduce_fds fds reducer res = 
  match fds with
  | [] -> res 
  | h :: t -> (
      let chan = Unix.in_channel_of_descr h in 
      let v = Marshal.from_channel chan in 
      reduce_fds t reducer (reducer res v)
  )

let rec consume_pipes pipes reducer res = 
  match pipes with 
  | [] -> res 
  | _ -> (
    let read, _wirte, _ex = Unix.select pipes [] [] (-1.0) in
    let _ = print_endline ".." in 
    let res = reduce_fds read reducer res in 
    List.iter Unix.close read;
    consume_pipes (remove pipes read) reducer res
  )

let parmap inp combiner reducer reducer_init = 
  let ncores = (Corecount.count () |> Nativeint.to_int) in 
  let parts = partition ncores inp in 
  let pipes = maprange (fun _n -> Unix.pipe ~cloexec:false ()) ncores in
  let _ = Array.iter2 (fun part (_pread, pwrite) -> spawn_worker combiner part pwrite) parts pipes in 
  consume_pipes (Array.to_list (Array.map (fun (pread, _pwrite) -> pread) pipes)) reducer reducer_init

let iter_word_histograms cluster_ids word_hists fname =
  iter_xml_pages fname (fun (_req : Warc.warc_entry) (headers : Warc.header) _head xml ->
    try (
        let meta_text = List.concat (List.map tokenize_text (channel_meta_text xml)) in 
        let url = Warc.get_url headers in 
        let cluster_id = url_cluster_id cluster_ids url in 
        let cluster_hist = Array.get word_hists cluster_id in 
        List.iter (fun word -> Art.incr cluster_hist word 1) meta_text;
    ) with Not_found -> ()
  ); ()

let word_histogram_worker cluster_ids word_hists fnames = 
  let _ = Array.iter (iter_word_histograms cluster_ids word_hists) fnames in 
  let _ = Printf.printf "%d" (Array.fold_left (+) 0 (Array.map Art.length word_hists)) in 
  let _ = print_endline "" in 
  Array.map (Art.items) word_hists

let word_histogram_reducer res hists = 
  (for i = 0 to ((Array.length res) - 1) do 
    let target = Array.get res i in 
    let src = Array.get hists i in 
    List.iter (fun (k, v) -> Art.incr target k v) src;
  done);
  res

let process_pages fnames clusters_fname outfname = 
  let clusters = load_cluster_ids clusters_fname in 
  let _cluster_hashes, cluster_ids = clusters in 
  let distinct_cluster_ids = Hashtbl.create 1024 in 
  for i = 0 to (Array1.dim cluster_ids) - 1 do 
    Hashtbl.replace distinct_cluster_ids (Array1.get cluster_ids i) 1;
  done;
  let n_clusters = Hashtbl.length distinct_cluster_ids in 
  let _ = Printf.printf "%d" n_clusters in 
  let _ = print_endline "" in 
  let first_hist = Art.create () in 
  let hists = Array.make n_clusters first_hist in 
  Array.set hists 0 first_hist;
  for i = 1 to (n_clusters - 1) do 
    Array.set hists i (Art.create ())
  done;
  let res = parmap fnames (word_histogram_worker clusters hists) word_histogram_reducer hists in 
  let out = open_out outfname in
  Marshal.to_channel out (Array.map Art.items res) [];
  close_out out; ()

let () = 
  let clusters_fname = Array.get Sys.argv 1 in 
  let outfname = Array.get Sys.argv 2 in 
  let fnames = Array.sub Sys.argv 3 ((Array.length Sys.argv) - 3) in 
  process_pages fnames clusters_fname outfname; ()
