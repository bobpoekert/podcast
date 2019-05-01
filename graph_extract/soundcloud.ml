open Utils

let is_some v = 
  match v with
  | None -> false
  | Some(_) -> true

let process_page agg page = 
  let data = Yojson.Basic.from_string page in 
  let open Yojson.Basic.Util in 
  let id = data |> member "user_id" |> to_int in 
  let items = (
    match data |> member "followers" with 
    | `List(v) -> Some((`Followers, v))
    | _ -> (
      match data |> member "followings" with 
      | `List(v) -> Some((`Following, v))
      | _ -> None
    )
  ) in 
  (match items with 
  | None -> ()
  | Some(direction, items) -> (
    let other_ids = items |> List.map (fun v -> v |> member "id" |> to_int_option) |> remove_none in 
    List.iter (fun other_id -> 
      let k = match direction with 
      | `Followers -> (id, other_id)
      | `Following -> (other_id, id) in 
      table_increment agg k;
    ) other_ids;
  ));
  agg

let process_pages agg infname = 
  fold_lines process_page agg (xunzip infname)

let rss_url soundcloud_id = 
  clean_url (Printf.sprintf "http://feeds.soundcloud.com/users/soundcloud:users:%d/sounds.rss" soundcloud_id)

let hash_pairs base_dirname = 
  let generator = line_seq (xz_files_stream base_dirname "*.jsons.xz") in 
  let combiner = Fork_combine.mappercombiner_no_stream process_page in 
  let reducer = Fork_combine.streamerreducer_no_stream (table_into_table (+)) in
  let combiner_initial = Hashtbl.create 100000 in 
  let pair_counts = Fork_combine.fork_combine
    ~generator: generator
    ~combiner: combiner
    ~combiner_initial_state: combiner_initial
    ~reducer: reducer 
    ~reducer_initial_state: combiner_initial in
  let n_res = Hashtbl.length pair_counts in 
  let res = Hashtbl.create n_res in 
  let res_urls = Hashtbl.create n_res in 
  Hashtbl.iter (fun (l, r) v -> 
    let url_left = rss_url l in 
    let url_right = rss_url r in 
    let hash_left = Murmur.murmur_hash url_left in 
    let hash_right = Murmur.murmur_hash url_right in 
    Hashtbl.add res (hash_left, hash_right) v;
    Hashtbl.replace res_urls url_left true;
    Hashtbl.replace res_urls url_right true;
  ) pair_counts;
  (table_keys res_urls, res)