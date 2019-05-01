open Soup
open Cohttp
open Utils

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

let rss_pattern = Re.Posix.compile_pat "<p class=\"subtit\"><a title=\"[^\"]*\" href=\"([^\"]+)\">"

let find_default h k d = 
  try 
    Hashtbl.find h k 
  with Not_found -> d

let find_option h k = 
  try (Some (Hashtbl.find h k)) with Not_found -> None

let process_page rss_mapping infname url_pairs = 
  Warc.iter_pages (gunzip infname) (fun inp ->
    match response_200 inp with 
    | None -> ()
    | Some(_meta, _header, body) -> (
      let rel_urls = extract_relation_urls body in 
      let urls = rel_urls in 
      (* let req_url = Warc.get_url meta in *)
      (* let urls = req_url :: urls in *)
      let urls = List.map clean_url urls in 
      let rss_urls = List.map (find_option rss_mapping) urls |> remove_none in 
      iter_pairwise (fun l r -> 
        table_increment url_pairs (l, r)
      ) rss_urls;
    )
  );
  url_pairs

let process_channel_page infname url_mapping = 
  Warc.iter_pages (gunzip infname) (fun page -> 
    match response_200 page with 
    | None -> () 
    | Some(meta, _heaer, body) -> (
      try (
        let req_url = Warc.get_url meta in 
        let rss_url = Re.Group.get (Re.exec rss_pattern body) 1 in 
        Hashtbl.replace url_mapping req_url rss_url;
      ) with Not_found -> ()
    )
  );
  url_mapping

let merge_table_reducer_str a b =
  let _ = table_into_table (fun a _b -> a) a b in a

let get_rss_url_mapping base_dirname = 
  let fnames = find_glob base_dirname "*.warc.gz" in 
  let combiner_initial = Hashtbl.create 100000 in 
  Parmap.parfold (reducer_catchall_r process_channel_page) (Parmap.L fnames) combiner_initial merge_table_reducer_str

let merge_table_reducer a b = 
  let _ = table_into_table (+) a b in a

let hash_pairs rss_mapping base_dirname = 
  let fnames = find_glob base_dirname "*.warc.gz" in 
  let combiner_initial = Hashtbl.create 100000 in 
  let pair_counts = Parmap.parfold (reducer_catchall_r (process_page rss_mapping)) (Parmap.L fnames) combiner_initial merge_table_reducer in 
  let urls = Hashtbl.create (Hashtbl.length pair_counts) in 
  let hash_pair_counts = Hashtbl.create (Hashtbl.length pair_counts) in 
  Hashtbl.iter (fun (l, r) v -> 
    Hashtbl.replace urls l true;
    Hashtbl.replace urls r true;
    Hashtbl.replace hash_pair_counts (Murmur.murmur_hash l, Murmur.murmur_hash r) v;
  ) pair_counts;
  let urls = table_keys urls in 
  (urls, hash_pair_counts)