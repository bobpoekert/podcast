open Lib.Rss
open Lib.Tokens
open Lib.Map_tile_types
open Lib.Map_tile_pb
open Utils
open Bigarray

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

let rec attrib_opt attrs target = 
  if attrs = [] then None else
  let k, v = (List.hd attrs) in 
  if k = target then (Some v) else
  attrib_opt (List.tl attrs) target

let text_content v =
  match v with 
  | Xml.PCData(v) -> v 
  | Xml.Element(_, _, _) -> ""

let text_content_of_children c = (Some (String.concat " " (List.map text_content c)))

let parse_rfc822 s = 
  (* taken from https://github.com/Cumulus/Syndic/blob/master/lib/syndic_date.ml *)
  Scanf.(
    let map f = function Some x -> f x | None -> None in
    let map2 f a b = match (a, b) with Some a, Some b -> f a b | _ -> None in
    let month_to_int = Hashtbl.create 12 in

    let make_date day month year h m maybe_s z =
        let month =
          if String.length month <= 3 then month else String.sub month 0 3
        in
        let month = Hashtbl.find month_to_int month in
        let date = Ptime.of_date (year, month, day) in
        let s =
          if maybe_s <> "" && maybe_s.[0] = ':' then
            float_of_string (String.sub maybe_s 1 (String.length maybe_s - 1))
          else 0.
        in
        let span = Ptime.Span.of_int_s ((h * 3600) + (m * 60)) in
        let span =
          map (fun x -> Some (Ptime.Span.add span x)) (Ptime.Span.of_float_s s)
        in
        let date_and_time =
          if z = "" || z = "GMT" || z = "UT" || z = "Z" then
            map2 (fun date span -> Ptime.add_span date span) date span
            |> map (fun x -> Some (Ptime.to_date_time x))
          else
            (* FIXME: this should be made more robust. *)
            let tz_offset_s =
              match z with
              | "EST" -> -5 * 3600
              | "EDT" -> -4 * 3600
              | "CST" -> -6 * 3600
              | "CDT" -> -5 * 3600
              | "MST" -> -7 * 3600
              | "MDT" -> -6 * 3600
              | "PST" -> -8 * 3600
              | "PDT" -> -7 * 3600
              | "A" -> -1 * 3600
              | "M" -> -12 * 3600
              | "N" -> 1 * 3600
              | "Y" -> 12 * 3600
              | _ ->
                  let zh = sscanf (String.sub z 0 3) "%i" (fun i -> i) in
                  let zm = sscanf (String.sub z 3 2) "%i" (fun i -> i) in
                  let tz_sign = if zh < 0 then -1 else 1 in
                  if zh < 0 then tz_sign * ((-zh * 3600) + (zm * 60))
                  else tz_sign * ((zh * 3600) + (zm * 60))
            in
            let rt = map2 (fun date span -> Ptime.add_span date span) date span in
            (* XXX: We lose minutes with this conversion, but Calendar does not
              propose to handle minutes. *)
            map (fun x -> Some (Ptime.to_date_time ~tz_offset_s x)) rt
        in
        match map Ptime.of_date_time date_and_time with
        | Some x -> x
        | None -> -1
      in
      try
        let res = if 'A' <= s.[0] && s.[0] <= 'Z' then
          try sscanf s "%_s %i %s %i %i:%i%s %s" make_date with _ ->
            try sscanf s "%_s %ist %s %i %i:%i%s %s" make_date with _ ->
              (* For e.g. "May 15th, 2019" — even though it is not standard *)
              sscanf s "%s %i%_s %i" (fun m d y -> make_date d m y 0 0 "" "UT")
        else
          try sscanf s "%i %s %i %i:%i%s %s" make_date with _ ->
            sscanf s "%i %s %i" (fun d m y -> make_date d m y 0 0 "" "UT") in 
        fst (Ptime.Span.to_d_ps (Ptime.to_span res))
      with _ -> -1
  )

let rec parse_rss_image children = 
  List.fold_left (fun (res:image) (child:Xml.xml) -> 
    match child with 
    | Xml.PCData(_) -> res 
    | Xml.Element(tag, _attrs, children) -> (
      match tag with 
      | "url" -> {res with url = text_content_of_children children} 
      | "title" -> {res with title = text_content_of_children children} 
      | "link" -> {res with href = text_content_of_children children}
      | _ -> res
    )
  ) (default_image ()) children 

let rec parse_itunes_categories tree = 
  match tree with 
  | Xml.PCData(_) -> None
  | Xml.Element(tag, attrs, children) -> (
    match tag with 
    | "itunes:category" -> (
      let value = attrib_opt attrs "text" in 
      let children = List.map parse_itunes_categories children in 
      Some (default_category ~name:value ~children:(filter_none_unwrap children) ())
    )
    | _ -> None
  )

let parse_owner (tags:Xml.xml list) = 
  List.fold_right (fun t (res:owner) ->
    match t with 
    | Xml.PCData(_) -> res 
    | Xml.Element(tagname, _attrs, children) -> 
      match tagname with 
      | "itunes:name" -> {res with name = text_content_of_children children} 
      | "itunes:email" -> {res with email = text_content_of_children children} 
      | _ -> res
  ) tags (default_owner ())

let mp3_extension_pat = Re.Posix.compile_pat ".*\.mp3.*"
let itunes_duration_pat = Re.Posix.compile_pat "\s*([\d\.]+):([\d\.]+):([\d\.]+)\s*"

let maybe_cons v l = 
  match v with 
  | None -> l 
  | Some(vv) -> vv :: l

let parse_tag_list s = 
    let t = String.split_on_char ',' s in 
    List.map (fun v -> default_tag ~name:(Some v) ()) t

let parse_episode tags = 
  let res = List.fold_left (fun res tag -> 
    match tag with 
    | Xml.PCData(_) -> res 
    | Xml.Element(tag_name, attrs, children) -> (
      match tag_name with 
      | "title" | "itunes:title" -> {res with titles = maybe_cons (text_content_of_children children) res.titles}
      | "pubDate" -> {res with pub_time = Some (Int64.of_int (parse_rfc822 (assert_some (text_content_of_children children))))} 
      | "guid" -> {res with publisher_guid = text_content_of_children children} 
      | "link" -> let url = text_content_of_children children |> assert_some in
        if Re.execp mp3_extension_pat url then res else {res with webpage_urls = url :: res.webpage_urls}
      | "itunes:image" -> {res with images = (default_image ~url:(attrib_opt attrs "href") ()) :: res.images} 
      | "description" | "content:encoded" -> {res with descriptions = maybe_cons (text_content_of_children children) res.descriptions}
      | "enclosure" -> {res with media_files = (
        default_media_file
          ~mime_type:(Some (Xml.attrib tag "type"))
          ~url:(attrib_opt attrs "url")
          ~length: (match attrib_opt attrs "length" with 
                    | None -> None 
                    | Some(s) -> float_of_string_opt s)
          ()
      ) :: res.media_files}
      | "itunes:duration" ->
        let s = text_content_of_children children in 
        let groups = Re.exec itunes_duration_pat s in 
        let groups = Re.Group.all groups |> Array.map float_of_string in 
        let hours = (Array.get groups 0) *. 3600000. in
        let minutes = (Array.get groups 1) *. 60000. in
        let seconds = (Array.get groups 2) *. 1000. in 
        let millis_total = hours +. minutes +. seconds in 
        {res with duration = Some millis_total}
      | "itunes:explicit" -> {res with explicit = text_content_of_children children} 
      | "itunes:keywords" -> {res with tags = List.append res.tags (parse_tag_list (assert_some (text_content_of_children children)))}
      | "itunes:subtitle" -> {res with subtitle = text_content_of_children children} 
      | "itunes:episode" -> {res with episode_number = text_content_of_children children}
      | "itunes:episodeType" -> let typ = text_content_of_children children |> assert_some |> String.trim |> String.lowercase_ascii in 
        {res with episode_type = match typ with 
          | "full" -> (Some Full) 
          | "trailer" -> (Some Trailer)
          | "bonus" -> (Some Bonus)
          | _ -> None
        }
      | "itunes:author" -> {res with author = (text_content_of_children children)}
      | _ -> res
    )
  ) (default_podcast_episode ()) tags in res


let rec _parse_rss_feed tree res:podcast = 
  match tree with 
  | Xml.PCData(_) -> res 
  | Xml.Element(tag, attrs, children) -> 
    match tag with 
    | "rss" | "channel" -> List.fold_right _parse_rss_feed children res
    | "title" -> {res with title = text_content_of_children children} 
    | "pubDate" -> {res with pub_date = (Some (Int64.of_int (parse_rfc822 (assert_some (text_content_of_children children)))))}
    | "lastBuildDate" -> {res with last_build_date = (Some (Int64.of_int (parse_rfc822 (assert_some (text_content_of_children children)))))}
    | "generator" -> {res with generator = text_content_of_children children}
    | "link" -> {res with homepage_url = text_content_of_children children} 
    | "language" -> {res with language = text_content_of_children children}
    | "copyright" -> {res with copyright = text_content_of_children children}
    | "managingEditor" -> {res with editor_contact = text_content_of_children children} 
    | "itunes:summary" -> {res with summary = text_content_of_children children} 
    | "image" -> {res with images = (parse_rss_image children) :: res.images}
    | "itunes:author" -> {res with author = text_content_of_children children} 
    | "itunes:keywords" -> {res with tags = List.append res.tags (parse_tag_list (assert_some (text_content_of_children children)))}
    | "itunes:category" -> {res with categories = maybe_cons (parse_itunes_categories tree) res.categories }
    | "itunes:image" -> {res with images = ((default_image ~url:(attrib_opt attrs "href")) ()) :: res.images} 
    | "itunes:explicit" -> {res with explicit = text_content_of_children children}
    | "itunes:owner" -> {res with owner = Some (parse_owner children)}
    | "description" -> {res with descriptions = maybe_cons (text_content_of_children children) res.descriptions} 
    | "itunes:subtitle" -> {res with slug = text_content_of_children children} 
    | "itunes:type" -> let v = text_content_of_children children |> assert_some |> String.trim in {res with is_serial = Some (v = "serial")}
    | "item" -> {res with episodes = (parse_episode children) :: res.episodes}
    | _ -> res


let parse_rss_feed tree = _parse_rss_feed tree (default_podcast ())

let maybe_cat sep a b = 
  match b with 
  | None -> a 
  | Some(vb) ->
    match a with 
    | None -> b 
    | Some(va) -> Some (va ^ sep ^ vb)

let blank_str_if_opt v = match str_opt with | None -> "" | Some(vv) -> vv

let string_of_tag (t:tag) = 
  (blank_str_if_opt t.name) ^ (blank_str_if_opt t.uri)

let rec string_of_category (c:category) : string = 
  (blank_str_if_opt c.name) ^ " " ^ (String.concat " " (List.map string_of_category c.children))

let text_from_episode (e:podcast_episode) = 
  (e.subtitle
  |> maybe_cat " " e.explicit
  |> maybe_cat " " e.subtitle
  |> maybe_cat " " e.author
  |> blank_str_if_opt)
  ^ (String.concat " " e.titles)
  ^ (String.concat " " e.descriptions) 
  ^ (String.concat " " e.webpage_urls)
  ^ (String.concat " " (List.map string_of_tag e.tags))


let text_from_podcast v = 
  (v.title
  |> maybe_cat " " v.slug
  |> maybe_cat " " v.homepage_url
  |> maybe_cat " " v.generator 
  |> maybe_cat " " v.language 
  |> maybe_cat " " v.editor_contact 
  |> maybe_cat " " v.copyright 
  |> maybe_cat " " v.summary 
  |> maybe_cat " " v.author
  |> blank_str_if_opt)
  ^ (String.concat " " v.descriptions) ^ " "
  ^ (String.concat " " (List.map string_of_tag v.tags)) ^ " "
  ^ (String.concat " " (List.map string_of_category v.categories)) ^ " "
  ^ (String.concat " " (List.map text_from_episode v.episodes))


let process_pages_worker fname_parts cluster_dists clusters_2d outfname core = 
  let fnames = Array.get fname_parts core in 
  let outfname = Printf.sprintf "%s-%d.bin" outfname core in 
  let outf = open_out_bin outfname in 
  Array.iter (fun fname ->
    iter_xml_pages fname (fun (_req : Warc.warc_entry) (headers : Warc.header) _head xml ->
      let url = Warc.get_url headers in 
      let id = url_hash url in 
      let proto_data = parse_rss_feed xml in 
      let meta_text = text_from_podcast proto_data in 
      let cluster_weights = cluster_distances cluster_dists meta_text in 
      let point_2d = fit_point_2d clusters_2d cluster_weights in 
      let loc = default_content_location (Array.to_list cluster_weights) (fst point_2d) (snd point_2d) in
      write_podcast_proto outf proto_res
    )
  ) fnames

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