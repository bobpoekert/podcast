open Utils

let find_opt h k =
  try Some(Hashtbl.find h k) with Not_found -> None

let table_into_table mergefn drain src =
  Hashtbl.iter (fun k v ->
    match find_opt drain k with 
    | None -> Hashtbl.add drain k v 
    | Some(v2) -> Hashtbl.replace drain k (mergefn v2 v)) src

let merge_table_reducer a b =
  match a with
  | None -> Some(b) 
  | Some(a_urls, a_pair_counts) -> (
    let b_urls, b_pair_counts = b in 
    table_into_table (fun a _ -> a) a_urls b_urls;
    table_into_table (+) a_pair_counts b_pair_counts;
    Some(a_urls, a_pair_counts)
  )

let table_values h = 
  let hlen = Hashtbl.length h in 
  match Hashtbl.fold (fun _k v acc -> 
    let off, acc = match acc with | Some(v) -> v | None -> (0, Array.make hlen v) in
    Array.set acc off v;
    Some(off + 1, acc)
  ) h None with 
  | None -> [| |]
  | Some(_, v) -> v

let table_increment t k =
  try
    (Hashtbl.replace t k ((Hashtbl.find t k) + 1); ())
  with Not_found ->
    (Hashtbl.replace t k 1; ())

let iter_pairwise thunk lst =
  List.iter (fun a -> 
    List.iter (fun b -> thunk a b) lst
  ) lst

let process_page agg page = 
  let id_to_url, id_pair_count = agg in
  let data = Yojson.Basic.from_string page in 
  let open Yojson.Basic.Util in 
  let html_data = data |> member "html_data" in 
  let ids_group = html_data 
    |> member "pageData" |> member "podcastPageData" |> member "listenersAlsoBought"
    |> to_list |> filter_string in 
  let itunes_id, rss_data = html_data
    |> member "storePlatformData" |> member "product-dv-product" |> member "results"
    |> to_assoc |> List.hd in 
  let rss_url = rss_data |> member "feedUrl" |> to_string in 
  let ids_group = itunes_id :: ids_group in 
  let ids_group = List.sort_uniq String.compare ids_group in
  Hashtbl.replace id_to_url itunes_id rss_url;
  iter_pairwise (fun a b -> 
    if String.compare a b < 0 then 
      table_increment id_pair_count (a, b)
  ) ids_group;
  agg

let assert_some v =
  match v with 
  | Some(v) -> v
  | None -> raise (Invalid_argument "unexpected None")

let sha_hash_pairs base_dirname =
  let generator = line_reader (xz_files_stream base_dirname "*.jsons.xz") in 
  let combiner = Fork_combine.mappercombiner_no_stream process_page in
  let reducer = Fork_combine.streamerreducer_no_stream merge_table_reducer in
  let combiner_initial = ((Hashtbl.create 1000), (Hashtbl.create 1000)) in
  let (id_to_url:((string, string) Hashtbl.t)), pair_counts = assert_some (Fork_combine.fork_combine 
    ~generator:generator
    ~combiner:combiner
    ~combiner_initial_state:combiner_initial
    ~reducer:reducer
    ~reducer_initial_state:None) in
  let sha_pair_counts = Hashtbl.create (Hashtbl.length pair_counts) in 
  Hashtbl.iter (fun (id_left, id_right) count ->
    let url_left = Hashtbl.find id_to_url id_left in 
    let url_right = Hashtbl.find id_to_url id_right in 
    let sha_left = Sha1.string url_left in 
    let sha_right = Sha1.string url_right in 
    Hashtbl.add sha_pair_counts (sha_left, sha_right) count
  ) pair_counts;
  (table_values id_to_url, sha_pair_counts)
