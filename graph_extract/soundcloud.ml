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
  | Some(_direction, items) -> (
    let other_ids = items |> List.map (fun v -> v |> member "id" |> to_int_option) |> remove_none in 
    let ids = id :: other_ids in 
    iter_pairwise (fun l r -> 
      table_increment agg (l, r)
    ) ids;
  ));
  agg

let process_pages infname agg =
  fold_lines (reducer_catchall process_page) agg (xunzip infname)

let rss_url soundcloud_id = 
  clean_url (Printf.sprintf "http://feeds.soundcloud.com/users/soundcloud:users:%d/sounds.rss" soundcloud_id)

let hash_pairs base_dirname = 
  let fnames = find_glob base_dirname "*.jsons.xz" in 
  let combiner_initial = Hashtbl.create 100000 in 
  let pair_counts = Parmap.parfold process_pages (Parmap.L fnames) combiner_initial (table_into_table (+)) in 
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