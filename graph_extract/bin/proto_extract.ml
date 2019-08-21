open Lib.Rss
open Lib.Tokens
open Utils
open Bigarray
open Map_tile_types
open Map_tile_pb

let cluster_into_tree inp = 
  let res = Art.create () in 
  let total = List.fold_left (fun total (_k, count) -> total + count) 0 inp |> float_of_int in 
  let _ = List.iter (fun (k, count) -> 
    Art.put res k count
  ) inp in 
  (res, total)

let tokens_into_tree tokens = 
  let res = Art.create () in 
  let _ = List.iter (fun k -> Art.incr res k 1) tokens in 
  res

let cluster_distance text_dist cluster_dist = 
  let text_sum = snd text_dist in 
  let cluster_sum = snd cluster_dist in 
  Hashtbl.fold (fun k text_count res -> 
    let text_count = float_of_int text_count in
    let text_prob = text_count /. text_sum in
    let cluster_prob = (float_of_int (Art.get (fst cluster_dist) k)) /. cluster_sum in 
    res -. (1.0 -. (abs_float (cluster_prob -. text_prob)))
  ) (fst text_dist)  1.0

let cluster_distances cluster_dists text =
  let tokens = List.concat (List.map tokenize_text text) in 
  let hist = Hashtbl.create (List.length tokens) in
  let _ = into_histogram hist tokens in 
  let text_sum = Hashtbl.fold (fun _k c res -> res + c) hist 0 |> float_of_int in
  let res = Array.map (cluster_distance (hist, text_sum)) cluster_dists in 
  let res_sum = Array.fold_left (+.) 0.0 res in 
  Array.map (fun v -> v /. res_sum) res

let fit_point_2d clusters_2d cluster_weights = 
  (* TODO: this should be real trilateration but we're just weighted averaging for now *)
  let res_x = ref 0.0 in 
  let res_y = ref 0.0 in (
    for i = 0 to (Array.length cluster_weights) do 
      let x = Array2.get clusters_2d i 0 in 
      let y = Array2.get clusters_2d i 1 in 
      let w = 1.0 -. (Array.get cluster_weights i) in (
        res_x := !res_x +. (x *. w);
        res_y := !res_y +. (y *. w);
      )
    done;
    (res_x, res_y)
  )

let item_text (item : Syndic_rss2.story) = 
  List.append [
    Tag(item.author);
    Tag(item.category)
  ] (match item.story with 
    | Syndic_rss2.All (title, _x, description) -> [Text(title); Text(description)]
    | Syndic_rss2.Title title -> [Text(title)]
    | Syndic_rss2.Descrption desc -> [Text(desc)])

let channel_text (feed : Syndic_rss2.channel) = 
  List.append [
    Text(feed.title);
    Text(feed.description);
    Tag(feed.category)
  ] (List.concat (List.map item_text feed.items))

let write_podcast_proto outf v = 
  let enc = Pbrt.Encoder.create () in
  let _ = encode_podcast v enc in
  let bs = Pbrt.Encoder.to_bytes enc in 
  let size = Bytes.length bs in (
    output_binary_int outf size;
    output_bytes outf bs;
  )

let pb_img img = 
  default_image
    ~url: Some(Uri.to_string img.url)
    ~title: Some(img.title) 
    ~width: Some(float_of_int img.width)
    ~height: Some(float_of_int img.height)
    ~use_context: Cover

let syndic_date_to_epoch d = 
  Syndic_date.epoch - 
    (
      (((Syndic_date.year d) - 1960) * 31557600)
      (((Syndic_date.month d) - 1) * 2629800) + 
      (Syndic_date.day d) * 86400 +
      (Syndic_date.hour d) * 3600 + 
      (Syndic_date.minute d) * 60 + 
      (Syndic_date.second d)
    )

let story_string s = 
  match s with 
  | Syndic_rss2.All (title, _x, desc) -> title ^ " " desc
  | Syndic_rss2.Title title -> title 
  | Syndic_rss2.Description (_x, desc) -> desc

let episode_id url ep = 
  match ep.guid with 
  | Some(v) -> url_hash v 
  | None -> (match ep.link with 
    | Some(v) -> url_hash v 
    | None -> (match ep.pub_date with 
      | Some(d) -> (url_hash url) lxor (syndic_date_to_epoch d)
      | None -> (url_hash url) lxor (story_string ep.story)
    )
  )

let title_string s = 
  match s with 
  | Syndic_rss2.All (title, _x, _desc) -> (Some title)
  | Syndic_rss2.Title title -> (Some title)
  | Syndic_rss2.Description _desc -> None

let description_string s = 
  match s with 
  | Syndic_rss2.All (_title, _x, desc) -> (Some desc)
  | Syndic_rss2.Title _title -> None
  | Syndic_rss2.Description desc -> (Some desc)


let pb_episode url ep = 
  default_podcast_episode
    ~id: episode_id url ep
    ~title: title_string ep.story 
    ~description description_string ep.story
    ~images: [] 
    ~media_files: 


let process_pages_worker fname_parts cluster_dists clusters_2d outfname core = 
  let fnames = Array.get fname_parts core in 
  let outfname = Printf.sprintf "%s-%d.bin" outfname core in 
  let outf = open_out_bin outfname in 
  Array.iter (fun fname ->
    Warc.iter_pages fname (fun page ->
      let req = Warc.get_req page in 
      let rsp = Warc.get_rsp page in
      let body = Warc.get_body rsp in
      let hrsp, body = Warc.parse_response_body body in
      let code = response_code hrsp in
      let headers = Warc.get_headers rsp in
      if code == 200 then (
        let feed = Xmlm.make_input (`String body) |> Syndic_rss2.parse in 
        let meta_text = channel_text feed in 
        let url = Warc.get_url headers in 
        let id = url_hash url in 
        let cluster_weights = cluster_distances cluster_dists channel_text in 
        let point_2d = fit_point_2d clusters_2d cluster_weights in 
        let loc = default_content_location (Array.to_list cluster_weights) (fst point_2d) (snd point_2d) in
        let proto_res = defaut_podcast 
          ~id:id 
          ~title: feed.title 
          ~description: feed.description 
          ~homepage_url: Uri.to_string feed.link 
          ~images: (match feed.image with | None -> [] | Some(img) -> [pb_image img])
          ~episodes: List.map pb_episode feed.items 
          ~location: (default_content_location ~weights: cluster_weights ~loc_x: (fst loc) ~loc_y: (snd loc)) in 
        write_podcast_proto outf proto_res
      )
      
    )
  ) fnames;
  close_out outf


let process_pages fnames clusters_fname clusters_2d_fname outfname =
  let clusters_fd = open_in clusters_fname in
  let dists = Marshal.from_channel clusters_fd in 
  let dists = Array.map (fun terms -> List.filter (fun (_k, count) -> count > 10) terms) dists in
  let dists_idxes = argfilter (fun l -> (List.length l) > 0) dists in 
  let dists = Array.map (Array.get dists) dists_idxes in 
  let dists = Array.map cluster_into_tree dists in 
  let _ = close_in clusters_fd in 
  let n_clusters = List.length cluster_dists in 
  let clusters_2d = load_array2 clusters_2d_fname Bigarray.Float32 n_clusters in 
  let fname_parts = partition (corecount ()) fnames in
  parrun (process_pages_worker fname_parts dists clusters_2d outfname)