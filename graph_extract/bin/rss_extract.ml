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

let splitter_re = Re.Pcre.regexp "[^a-zA-Z0-9]+"

let rec _clean_tokens tokens res =
  match tokens with
  | [] -> res
  | h :: t ->
    match h with
    | `Text(v) -> _clean_tokens t ((String.lowercase_ascii v) :: res)
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

type tree = (Art.tree * int)

let into_tree words : tree =
  let res = Art.create () in 
  List.iter (fun word -> Art.incr res word 1) words;
  (res, Art.sum res)

let load_tree fname : tree array = 
  let chan = open_in fname in 
  let data = Marshal.from_channel chan in 
  let _ = close_file chan in 
  Array.map (fun rows -> 
    let tree = Art.create () in 
    List.iter (fun (k, v) -> Art.put tree k v) rows;
    (tree, Art.sum tree)
  ) data


let tree_similarity a b =
  let a, a_sum = a in
  let b, _b_sum = b in 
  Art.fold a (fun k v res -> 
    try (
      let vb = Art.get b k in
      res - abs (v - vb)
    ) with Not_found -> res
  ) a_sum


let pairwise_tree_similarities target trees = 
  let n_trees = (Array.length trees) - 1 in
  let batch_size = n_trees / (corecount ()) in 
  parrun (fun batch ->
    let start = batch_size * batch in 
    for l = start to (start + batch_size) do 
      for r = 0 to n_trees do 
        if l != r then (
          Array2.set target l r (Int64.of_int (tree_similarity (Array.get trees l) (Array.get trees r)))
        )
      done
    done;
  );
  target

let pairwise_tree_similarities_to_file fname trees = 
  let dim_size = Array.length trees in 
  let _ = array2_with_file fname Int64 dim_size dim_size (fun arr -> pairwise_tree_similarities arr trees) in ()

let words_vec hists words = 
  let wt = into_tree words in 
  let sims = Array.map (tree_similarity wt) hists in 
  let total_sim = float_of_int (Array.fold_left (+) 0 sims) in 
  Array.map (fun v -> (float_of_int v) /. total_sim) sims

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

let pack_i32 v = 
  let l = Int32.to_int v in 
  let r = Int32.to_int (Int32.shift_right_logical v 24) in
  let res = Bytes.make 4 ' ' in 
  Bytes.set res 0 (Char.unsafe_chr (l land 0xFF));
  Bytes.set res 1 (Char.unsafe_chr ((l lsr 8) land 0xFF));
  Bytes.set res 2 (Char.unsafe_chr ((l lsr 16) land 0xFF));
  Bytes.set res 3 (Char.unsafe_chr (r land 0xFF));
  Bytes.to_string res

let pack_float32 v = 
  let bits = Int32.bits_of_float v in 
  pack_i32 bits

let pack_int64 v =
  (pack_i32 (Int64.to_int32 v)) ^ (pack_i32 (Int64.to_int32 (Int64.shift_right_logical v 32)))

let generate_page_vecs outfname infnames trees = 
  let ncores = (Corecount.count () |> Nativeint.to_int) in 
  let parts = partition ncores infnames in 
  let pids = Array.mapi (fun i part ->
    let pid = Unix.fork () in 
    if pid = 0 then (
      let outf = open_out (Printf.sprintf "%s.%d" outfname i) in 
      Array.iter (fun xml_fname -> 
        iter_xml_pages xml_fname (fun (_req: Warc.warc_entry) (headers: Warc.header) _head xml ->
          let meta_text = List.filter (fun s -> (String.length s) < 25) (
            List.concat (List.map tokenize_text (channel_meta_text xml))
          ) in 
          let page_tree = into_tree meta_text in 
          let vec = Array.map (tree_similarity page_tree) trees in
          let vec_sum = Array.fold_left (+) 0 vec |> Float.of_int in 
          let vec = Array.map (fun v -> (Float.of_int v) /. vec_sum) vec in 
          let id = url_hash (Warc.get_url headers) |> Int64.of_int in 
          output_string outf (pack_int64 id);
          Array.iter (fun v -> output_string outf (pack_float32 v)) vec;
        )
      ) part;
      close_out outf;
      exit 0;
    ) else pid
  ) parts in 
  Array.iter (fun pid -> let _ = Unix.waitpid [] pid in ()) pids; ()


let process_pages fnames clusters_fname outfname pairwise_outfname vecs_outfname = 
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
  let res = array_filteri (fun _i v -> (Art.length v) > 1) res in 
  let res_trees = Array.map (fun t -> (t, Art.sum t)) res in 
  let out = open_out outfname in
  Marshal.to_channel out (Array.map Art.items res) [];
  close_out out;
  pairwise_tree_similarities_to_file pairwise_outfname res_trees;
  generate_page_vecs vecs_outfname fnames res_trees; ()

let () = 
  let clusters_fname = Array.get Sys.argv 1 in 
  let outfname = Array.get Sys.argv 2 in 
  let pairwise_outfname = Array.get Sys.argv 3 in
  let vecs_outfname = Array.get Sys.argv 4 in 
  let fnames = Array.sub Sys.argv 5 ((Array.length Sys.argv) - 5) in 
  process_pages fnames clusters_fname outfname pairwise_outfname vecs_outfname; ()
