open Utils


let merge_table_reducer a b =
  match a with
  | None -> Some(b) 
  | Some(a_urls, a_pair_counts) -> (
    let b_urls, b_pair_counts = b in 
    let _ = table_into_table (fun a _ -> a) a_urls b_urls in 
    let _ = table_into_table (+) a_pair_counts b_pair_counts in 
    Some(a_urls, a_pair_counts)
  )

let process_page agg page = 
  let id_to_url, id_pair_count = agg in
  let data = Yojson.Basic.from_string page in 
  let open Yojson.Basic.Util in 
  let html_data = data |> member "html_data" in
  if html_data != `Null then (
      let ids_group = html_data 
        |> member "pageData" |> member "podcastPageData" |> member "listenersAlsoBought"
        |> to_list |> filter_string in 
      let itunes_id, rss_data = html_data
        |> member "storePlatformData" |> member "product-dv-product" |> member "results"
        |> to_assoc |> List.hd in 
      let rss_url = rss_data |> member "feedUrl" |> to_string |> clean_url in
      let ids_group = itunes_id :: ids_group in 
      let ids_group = List.sort_uniq String.compare ids_group in
      Hashtbl.replace id_to_url itunes_id rss_url;
      iter_pairwise (fun a b -> 
          table_increment id_pair_count (a, b)
      ) ids_group;
  );
  agg

let process_pages agg infname =
  fold_lines process_page agg (xunzip infname)

let hash_pairs base_dirname =
  let generator = line_seq (xz_files_stream base_dirname "*.jsons.xz") in 
  let combiner = Fork_combine.mappercombiner_no_stream process_page in
  let reducer = Fork_combine.streamerreducer_no_stream merge_table_reducer in
  let combiner_initial = ((Hashtbl.create 1000), (Hashtbl.create 1000)) in
  let id_to_url, pair_counts = assert_some (Fork_combine.fork_combine 
    ~generator:generator
    ~combiner:combiner
    ~combiner_initial_state:combiner_initial
    ~reducer:reducer
    ~reducer_initial_state:None) in
  let hash_pair_counts = Hashtbl.create (Hashtbl.length pair_counts) in 
  let urls = table_values id_to_url in 
  Hashtbl.iter (fun (id_left, id_right) count ->
    try
      let url_left = Hashtbl.find id_to_url id_left in 
      let url_right = Hashtbl.find id_to_url id_right in
      let hash_left = Murmur.murmur_hash url_left in 
      let hash_right = Murmur.murmur_hash url_right in
      let pair = if hash_left < hash_right then (hash_left, hash_right) else (hash_right, hash_left) in 
      Hashtbl.add hash_pair_counts pair count
    with Not_found -> ()
  ) pair_counts;
  (urls, hash_pair_counts)
