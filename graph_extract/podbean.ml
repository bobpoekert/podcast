open Soup
open Cohttp
open Utils

let warc_generator instream = 
  let rec res () = (try Some(Warc.next_page instream) with 
    |End_of_file -> None 
    |Warc.Incomplete_request -> res ()) in 
  res

let response_200 page = 
  let rsp = Warc.get_rsp page in 
  let body = Warc.get_body rsp in
  let rsp_head, rsp_body = Warc.parse_response_body body in 
  let code = Code.code_of_status (Response.status rsp_head) in 
  if code == 200 then Some(Warc.get_headers rsp, rsp_head, rsp_body) else None

let get_hrefs nodes = 
  nodes |> to_list |> List.map (fun v -> clean_url (R.attribute "href" v))

let relation_pattern = Re.Posix.compile_pat "<a href=\"([^\"]+)\" class=\"pic\">"

let extract_relation_urls page =
  page
  |> Re.all relation_pattern
  |> List.map (fun v -> Re.Group.get v 1)

let process_page url_pairs inp = 
  match response_200 inp with 
  | None -> url_pairs
  | Some(meta, _header, body) -> (
    let req_url = Warc.get_url meta in 
    let rel_urls = extract_relation_urls body in 
    let urls = req_url :: rel_urls in 
    let urls = List.map clean_url urls in 
    iter_pairwise (fun l r -> 
      table_increment url_pairs (l, r)
    ) urls;
    url_pairs
  )

let merge_table_reducer a b = 
  match a with 
  | None -> Some(b)
  | Some(a_pairs) -> (
    let _ = table_into_table (+) a_pairs b in a
  )

let hash_pairs base_dirname = 
  let generator = warc_generator (gzip_files_stream base_dirname "*.warc.gz") in 
  let combiner = Fork_combine.mappercombiner_no_stream process_page in 
  let reducer = Fork_combine.streamerreducer_no_stream merge_table_reducer in 
  let combiner_initial = Hashtbl.create 1000 in 
  let pair_counts = assert_some (Fork_combine.fork_combine 
    ~generator: generator
    ~combiner: combiner
    ~reducer: reducer
    ~combiner_initial_state: combiner_initial 
    ~reducer_initial_state: None
  ) in
  let urls = Hashtbl.create (Hashtbl.length pair_counts) in 
  let hash_pair_counts = Hashtbl.create (Hashtbl.length pair_counts) in 
  Hashtbl.iter (fun (l, r) v -> 
    Hashtbl.replace urls l true;
    Hashtbl.replace urls r true;
    Hashtbl.replace hash_pair_counts (Murmur.murmur_hash l, Murmur.murmur_hash r) v;
  ) pair_counts;
  let urls = table_keys urls in 
  (urls, hash_pair_counts)