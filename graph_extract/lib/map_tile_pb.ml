[@@@ocaml.warning "-27-30-39"]

type image_mutable = {
  mutable url : string option;
  mutable mime_type : string option;
  mutable width : float option;
  mutable height : float option;
  mutable title : string option;
  mutable href : string option;
  mutable x_offset : float option;
  mutable y_offset : float option;
  mutable use_context : Map_tile_types.image_use_context option;
}

let default_image_mutable () : image_mutable = {
  url = None;
  mime_type = None;
  width = None;
  height = None;
  title = None;
  href = None;
  x_offset = None;
  y_offset = None;
  use_context = None;
}

type tag_mutable = {
  mutable name : string option;
  mutable uri : string option;
}

let default_tag_mutable () : tag_mutable = {
  name = None;
  uri = None;
}

type media_file_mutable = {
  mutable url : string option;
  mutable mime_type : string option;
  mutable media_type : Map_tile_types.media_file_media_type option;
  mutable length : float option;
}

let default_media_file_mutable () : media_file_mutable = {
  url = None;
  mime_type = None;
  media_type = None;
  length = None;
}

type content_location_mutable = {
  mutable weights : float list;
  mutable loc_x : float option;
  mutable loc_y : float option;
}

let default_content_location_mutable () : content_location_mutable = {
  weights = [];
  loc_x = None;
  loc_y = None;
}

type category_mutable = {
  mutable name : string option;
  mutable children : Map_tile_types.category list;
}

let default_category_mutable () : category_mutable = {
  name = None;
  children = [];
}

type owner_mutable = {
  mutable name : string option;
  mutable email : string option;
}

let default_owner_mutable () : owner_mutable = {
  name = None;
  email = None;
}

type podcast_episode_mutable = {
  mutable id : int64;
  mutable titles : string list;
  mutable descriptions : string list;
  mutable images : Map_tile_types.image list;
  mutable media_files : Map_tile_types.media_file list;
  mutable tags : Map_tile_types.tag list;
  mutable location : Map_tile_types.content_location option;
  mutable pub_time : int64 option;
  mutable publisher_guid : string option;
  mutable webpage_urls : string list;
  mutable duration : float option;
  mutable explicit : string option;
  mutable subtitle : string option;
  mutable episode_number : string option;
  mutable episode_type : Map_tile_types.podcast_episode_episode_type option;
  mutable author : string option;
}

let default_podcast_episode_mutable () : podcast_episode_mutable = {
  id = 0L;
  titles = [];
  descriptions = [];
  images = [];
  media_files = [];
  tags = [];
  location = None;
  pub_time = None;
  publisher_guid = None;
  webpage_urls = [];
  duration = None;
  explicit = None;
  subtitle = None;
  episode_number = None;
  episode_type = None;
  author = None;
}

type podcast_mutable = {
  mutable id : int64;
  mutable title : string option;
  mutable slug : string option;
  mutable descriptions : string list;
  mutable homepage_url : string option;
  mutable images : Map_tile_types.image list;
  mutable episodes : Map_tile_types.podcast_episode list;
  mutable location : Map_tile_types.content_location option;
  mutable tags : Map_tile_types.tag list;
  mutable pub_date : int64 option;
  mutable last_build_date : int64 option;
  mutable generator : string option;
  mutable language : string option;
  mutable copyright : string option;
  mutable editor_contact : string option;
  mutable summary : string option;
  mutable author : string option;
  mutable categories : Map_tile_types.category list;
  mutable explicit : string option;
  mutable owner : Map_tile_types.owner option;
  mutable is_serial : bool option;
}

let default_podcast_mutable () : podcast_mutable = {
  id = 0L;
  title = None;
  slug = None;
  descriptions = [];
  homepage_url = None;
  images = [];
  episodes = [];
  location = None;
  tags = [];
  pub_date = None;
  last_build_date = None;
  generator = None;
  language = None;
  copyright = None;
  editor_contact = None;
  summary = None;
  author = None;
  categories = [];
  explicit = None;
  owner = None;
  is_serial = None;
}

type term_mutable = {
  mutable value : string;
  mutable score : float option;
  mutable x : float option;
  mutable y : float option;
}

let default_term_mutable () : term_mutable = {
  value = "";
  score = None;
  x = None;
  y = None;
}

type cluster_seed_mutable = {
  mutable x : float;
  mutable y : float;
  mutable label : int32;
}

let default_cluster_seed_mutable () : cluster_seed_mutable = {
  x = 0.;
  y = 0.;
  label = 0l;
}

type delta_mutable = {
  mutable dx : float;
  mutable dy : float;
}

let default_delta_mutable () : delta_mutable = {
  dx = 0.;
  dy = 0.;
}

type path_mutable = {
  mutable start_x : float;
  mutable start_y : float;
  mutable deltas : Map_tile_types.delta list;
}

let default_path_mutable () : path_mutable = {
  start_x = 0.;
  start_y = 0.;
  deltas = [];
}

type map_tile_mutable = {
  mutable min_x : float;
  mutable max_x : float;
  mutable min_y : float;
  mutable max_y : float;
  mutable podcasts : Map_tile_types.podcast list;
  mutable terms : Map_tile_types.term list;
  mutable paths : Map_tile_types.path list;
  mutable seeds : Map_tile_types.cluster_seed list;
}

let default_map_tile_mutable () : map_tile_mutable = {
  min_x = 0.;
  max_x = 0.;
  min_y = 0.;
  max_y = 0.;
  podcasts = [];
  terms = [];
  paths = [];
  seeds = [];
}


let rec decode_image_use_context d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Map_tile_types.Thumbnail:Map_tile_types.image_use_context)
  | 1 -> (Map_tile_types.Cover:Map_tile_types.image_use_context)
  | 2 -> (Map_tile_types.Favicon:Map_tile_types.image_use_context)
  | 3 -> (Map_tile_types.App_icon:Map_tile_types.image_use_context)
  | 3 -> (Map_tile_types.Avatar:Map_tile_types.image_use_context)
  | _ -> Pbrt.Decoder.malformed_variant "image_use_context"

let rec decode_image d =
  let v = default_image_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.url <- Some (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.mime_type <- Some (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(2)" pk
    | Some (3, Pbrt.Bits32) -> begin
      v.width <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(3)" pk
    | Some (4, Pbrt.Bits32) -> begin
      v.height <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(4)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.title <- Some (Pbrt.Decoder.string d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.href <- Some (Pbrt.Decoder.string d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(9)" pk
    | Some (6, Pbrt.Bits32) -> begin
      v.x_offset <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(6)" pk
    | Some (7, Pbrt.Bits32) -> begin
      v.y_offset <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(7)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.use_context <- Some (decode_image_use_context d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(image), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.url = v.url;
    Map_tile_types.mime_type = v.mime_type;
    Map_tile_types.width = v.width;
    Map_tile_types.height = v.height;
    Map_tile_types.title = v.title;
    Map_tile_types.href = v.href;
    Map_tile_types.x_offset = v.x_offset;
    Map_tile_types.y_offset = v.y_offset;
    Map_tile_types.use_context = v.use_context;
  } : Map_tile_types.image)

let rec decode_tag d =
  let v = default_tag_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Some (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.uri <- Some (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tag), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.name = v.name;
    Map_tile_types.uri = v.uri;
  } : Map_tile_types.tag)

let rec decode_media_file_media_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Map_tile_types.Audio:Map_tile_types.media_file_media_type)
  | 2 -> (Map_tile_types.Video:Map_tile_types.media_file_media_type)
  | 3 -> (Map_tile_types.Other:Map_tile_types.media_file_media_type)
  | _ -> Pbrt.Decoder.malformed_variant "media_file_media_type"

let rec decode_media_file d =
  let v = default_media_file_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.url <- Some (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(media_file), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.mime_type <- Some (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(media_file), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.media_type <- Some (decode_media_file_media_type d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(media_file), field(3)" pk
    | Some (4, Pbrt.Bits32) -> begin
      v.length <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(media_file), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.url = v.url;
    Map_tile_types.mime_type = v.mime_type;
    Map_tile_types.media_type = v.media_type;
    Map_tile_types.length = v.length;
  } : Map_tile_types.media_file)

let rec decode_content_location d =
  let v = default_content_location_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.weights <- List.rev v.weights;
    ); continue__ := false
    | Some (8, Pbrt.Bits32) -> begin
      v.weights <- (Pbrt.Decoder.float_as_bits32 d) :: v.weights;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(content_location), field(8)" pk
    | Some (9, Pbrt.Bits32) -> begin
      v.loc_x <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(content_location), field(9)" pk
    | Some (10, Pbrt.Bits32) -> begin
      v.loc_y <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(content_location), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.weights = v.weights;
    Map_tile_types.loc_x = v.loc_x;
    Map_tile_types.loc_y = v.loc_y;
  } : Map_tile_types.content_location)

let rec decode_category d =
  let v = default_category_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.children <- List.rev v.children;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Some (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(category), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.children <- (decode_category (Pbrt.Decoder.nested d)) :: v.children;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(category), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.name = v.name;
    Map_tile_types.children = v.children;
  } : Map_tile_types.category)

let rec decode_owner d =
  let v = default_owner_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Some (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(owner), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.email <- Some (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(owner), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Map_tile_types.name = v.name;
    Map_tile_types.email = v.email;
  } : Map_tile_types.owner)

let rec decode_podcast_episode_episode_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 1 -> (Map_tile_types.Full:Map_tile_types.podcast_episode_episode_type)
  | 2 -> (Map_tile_types.Trailer:Map_tile_types.podcast_episode_episode_type)
  | 3 -> (Map_tile_types.Bonus:Map_tile_types.podcast_episode_episode_type)
  | _ -> Pbrt.Decoder.malformed_variant "podcast_episode_episode_type"

let rec decode_podcast_episode d =
  let v = default_podcast_episode_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.webpage_urls <- List.rev v.webpage_urls;
      v.tags <- List.rev v.tags;
      v.media_files <- List.rev v.media_files;
      v.images <- List.rev v.images;
      v.descriptions <- List.rev v.descriptions;
      v.titles <- List.rev v.titles;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int64_as_varint d; id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.titles <- (Pbrt.Decoder.string d) :: v.titles;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.descriptions <- (Pbrt.Decoder.string d) :: v.descriptions;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.images <- (decode_image (Pbrt.Decoder.nested d)) :: v.images;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.media_files <- (decode_media_file (Pbrt.Decoder.nested d)) :: v.media_files;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.tags <- (decode_tag (Pbrt.Decoder.nested d)) :: v.tags;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.location <- Some (decode_content_location (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(7)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.pub_time <- Some (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.publisher_guid <- Some (Pbrt.Decoder.string d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.webpage_urls <- (Pbrt.Decoder.string d) :: v.webpage_urls;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(10)" pk
    | Some (11, Pbrt.Bits64) -> begin
      v.duration <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(11)" pk
    | Some (12, Pbrt.Bytes) -> begin
      v.explicit <- Some (Pbrt.Decoder.string d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.subtitle <- Some (Pbrt.Decoder.string d);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(13)" pk
    | Some (14, Pbrt.Bytes) -> begin
      v.episode_number <- Some (Pbrt.Decoder.string d);
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(14)" pk
    | Some (15, Pbrt.Varint) -> begin
      v.episode_type <- Some (decode_podcast_episode_episode_type d);
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(15)" pk
    | Some (16, Pbrt.Bytes) -> begin
      v.author <- Some (Pbrt.Decoder.string d);
    end
    | Some (16, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast_episode), field(16)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Map_tile_types.id = v.id;
    Map_tile_types.titles = v.titles;
    Map_tile_types.descriptions = v.descriptions;
    Map_tile_types.images = v.images;
    Map_tile_types.media_files = v.media_files;
    Map_tile_types.tags = v.tags;
    Map_tile_types.location = v.location;
    Map_tile_types.pub_time = v.pub_time;
    Map_tile_types.publisher_guid = v.publisher_guid;
    Map_tile_types.webpage_urls = v.webpage_urls;
    Map_tile_types.duration = v.duration;
    Map_tile_types.explicit = v.explicit;
    Map_tile_types.subtitle = v.subtitle;
    Map_tile_types.episode_number = v.episode_number;
    Map_tile_types.episode_type = v.episode_type;
    Map_tile_types.author = v.author;
  } : Map_tile_types.podcast_episode)

let rec decode_podcast d =
  let v = default_podcast_mutable () in
  let continue__= ref true in
  let id_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.categories <- List.rev v.categories;
      v.tags <- List.rev v.tags;
      v.episodes <- List.rev v.episodes;
      v.images <- List.rev v.images;
      v.descriptions <- List.rev v.descriptions;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.id <- Pbrt.Decoder.int64_as_varint d; id_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.title <- Some (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.slug <- Some (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.descriptions <- (Pbrt.Decoder.string d) :: v.descriptions;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.homepage_url <- Some (Pbrt.Decoder.string d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.images <- (decode_image (Pbrt.Decoder.nested d)) :: v.images;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.episodes <- (decode_podcast_episode (Pbrt.Decoder.nested d)) :: v.episodes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.location <- Some (decode_content_location (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.tags <- (decode_tag (Pbrt.Decoder.nested d)) :: v.tags;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.pub_date <- Some (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(10)" pk
    | Some (11, Pbrt.Varint) -> begin
      v.last_build_date <- Some (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(11)" pk
    | Some (12, Pbrt.Bytes) -> begin
      v.generator <- Some (Pbrt.Decoder.string d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.language <- Some (Pbrt.Decoder.string d);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(13)" pk
    | Some (14, Pbrt.Bytes) -> begin
      v.copyright <- Some (Pbrt.Decoder.string d);
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(14)" pk
    | Some (15, Pbrt.Bytes) -> begin
      v.editor_contact <- Some (Pbrt.Decoder.string d);
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(15)" pk
    | Some (16, Pbrt.Bytes) -> begin
      v.summary <- Some (Pbrt.Decoder.string d);
    end
    | Some (16, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(16)" pk
    | Some (17, Pbrt.Bytes) -> begin
      v.author <- Some (Pbrt.Decoder.string d);
    end
    | Some (17, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(17)" pk
    | Some (18, Pbrt.Bytes) -> begin
      v.categories <- (decode_category (Pbrt.Decoder.nested d)) :: v.categories;
    end
    | Some (18, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(18)" pk
    | Some (19, Pbrt.Bytes) -> begin
      v.explicit <- Some (Pbrt.Decoder.string d);
    end
    | Some (19, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(19)" pk
    | Some (20, Pbrt.Bytes) -> begin
      v.owner <- Some (decode_owner (Pbrt.Decoder.nested d));
    end
    | Some (20, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(20)" pk
    | Some (21, Pbrt.Varint) -> begin
      v.is_serial <- Some (Pbrt.Decoder.bool d);
    end
    | Some (21, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(podcast), field(21)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !id_is_set then Pbrt.Decoder.missing_field "id" end;
  ({
    Map_tile_types.id = v.id;
    Map_tile_types.title = v.title;
    Map_tile_types.slug = v.slug;
    Map_tile_types.descriptions = v.descriptions;
    Map_tile_types.homepage_url = v.homepage_url;
    Map_tile_types.images = v.images;
    Map_tile_types.episodes = v.episodes;
    Map_tile_types.location = v.location;
    Map_tile_types.tags = v.tags;
    Map_tile_types.pub_date = v.pub_date;
    Map_tile_types.last_build_date = v.last_build_date;
    Map_tile_types.generator = v.generator;
    Map_tile_types.language = v.language;
    Map_tile_types.copyright = v.copyright;
    Map_tile_types.editor_contact = v.editor_contact;
    Map_tile_types.summary = v.summary;
    Map_tile_types.author = v.author;
    Map_tile_types.categories = v.categories;
    Map_tile_types.explicit = v.explicit;
    Map_tile_types.owner = v.owner;
    Map_tile_types.is_serial = v.is_serial;
  } : Map_tile_types.podcast)

let rec decode_term d =
  let v = default_term_mutable () in
  let continue__= ref true in
  let value_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- Pbrt.Decoder.string d; value_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(term), field(1)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.score <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(term), field(2)" pk
    | Some (3, Pbrt.Bits32) -> begin
      v.x <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(term), field(3)" pk
    | Some (4, Pbrt.Bits32) -> begin
      v.y <- Some (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(term), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !value_is_set then Pbrt.Decoder.missing_field "value" end;
  ({
    Map_tile_types.value = v.value;
    Map_tile_types.score = v.score;
    Map_tile_types.x = v.x;
    Map_tile_types.y = v.y;
  } : Map_tile_types.term)

let rec decode_cluster_seed d =
  let v = default_cluster_seed_mutable () in
  let continue__= ref true in
  let label_is_set = ref false in
  let y_is_set = ref false in
  let x_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.x <- Pbrt.Decoder.float_as_bits32 d; x_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(cluster_seed), field(1)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.y <- Pbrt.Decoder.float_as_bits32 d; y_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(cluster_seed), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.label <- Pbrt.Decoder.int32_as_varint d; label_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(cluster_seed), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !label_is_set then Pbrt.Decoder.missing_field "label" end;
  begin if not !y_is_set then Pbrt.Decoder.missing_field "y" end;
  begin if not !x_is_set then Pbrt.Decoder.missing_field "x" end;
  ({
    Map_tile_types.x = v.x;
    Map_tile_types.y = v.y;
    Map_tile_types.label = v.label;
  } : Map_tile_types.cluster_seed)

let rec decode_delta d =
  let v = default_delta_mutable () in
  let continue__= ref true in
  let dy_is_set = ref false in
  let dx_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.dx <- Pbrt.Decoder.float_as_bits32 d; dx_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(delta), field(1)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.dy <- Pbrt.Decoder.float_as_bits32 d; dy_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(delta), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !dy_is_set then Pbrt.Decoder.missing_field "dy" end;
  begin if not !dx_is_set then Pbrt.Decoder.missing_field "dx" end;
  ({
    Map_tile_types.dx = v.dx;
    Map_tile_types.dy = v.dy;
  } : Map_tile_types.delta)

let rec decode_path d =
  let v = default_path_mutable () in
  let continue__= ref true in
  let start_y_is_set = ref false in
  let start_x_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.deltas <- List.rev v.deltas;
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.start_x <- Pbrt.Decoder.float_as_bits32 d; start_x_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(path), field(1)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.start_y <- Pbrt.Decoder.float_as_bits32 d; start_y_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(path), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.deltas <- (decode_delta (Pbrt.Decoder.nested d)) :: v.deltas;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(path), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !start_y_is_set then Pbrt.Decoder.missing_field "start_y" end;
  begin if not !start_x_is_set then Pbrt.Decoder.missing_field "start_x" end;
  ({
    Map_tile_types.start_x = v.start_x;
    Map_tile_types.start_y = v.start_y;
    Map_tile_types.deltas = v.deltas;
  } : Map_tile_types.path)

let rec decode_map_tile d =
  let v = default_map_tile_mutable () in
  let continue__= ref true in
  let max_y_is_set = ref false in
  let min_y_is_set = ref false in
  let max_x_is_set = ref false in
  let min_x_is_set = ref false in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.seeds <- List.rev v.seeds;
      v.paths <- List.rev v.paths;
      v.terms <- List.rev v.terms;
      v.podcasts <- List.rev v.podcasts;
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.min_x <- Pbrt.Decoder.float_as_bits32 d; min_x_is_set := true;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(1)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.max_x <- Pbrt.Decoder.float_as_bits32 d; max_x_is_set := true;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(2)" pk
    | Some (3, Pbrt.Bits32) -> begin
      v.min_y <- Pbrt.Decoder.float_as_bits32 d; min_y_is_set := true;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(3)" pk
    | Some (4, Pbrt.Bits32) -> begin
      v.max_y <- Pbrt.Decoder.float_as_bits32 d; max_y_is_set := true;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.podcasts <- (decode_podcast (Pbrt.Decoder.nested d)) :: v.podcasts;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.terms <- (decode_term (Pbrt.Decoder.nested d)) :: v.terms;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.paths <- (decode_path (Pbrt.Decoder.nested d)) :: v.paths;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.seeds <- (decode_cluster_seed (Pbrt.Decoder.nested d)) :: v.seeds;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(map_tile), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  begin if not !max_y_is_set then Pbrt.Decoder.missing_field "max_y" end;
  begin if not !min_y_is_set then Pbrt.Decoder.missing_field "min_y" end;
  begin if not !max_x_is_set then Pbrt.Decoder.missing_field "max_x" end;
  begin if not !min_x_is_set then Pbrt.Decoder.missing_field "min_x" end;
  ({
    Map_tile_types.min_x = v.min_x;
    Map_tile_types.max_x = v.max_x;
    Map_tile_types.min_y = v.min_y;
    Map_tile_types.max_y = v.max_y;
    Map_tile_types.podcasts = v.podcasts;
    Map_tile_types.terms = v.terms;
    Map_tile_types.paths = v.paths;
    Map_tile_types.seeds = v.seeds;
  } : Map_tile_types.map_tile)

let rec encode_image_use_context (v:Map_tile_types.image_use_context) encoder =
  match v with
  | Map_tile_types.Thumbnail -> Pbrt.Encoder.int_as_varint (0) encoder
  | Map_tile_types.Cover -> Pbrt.Encoder.int_as_varint 1 encoder
  | Map_tile_types.Favicon -> Pbrt.Encoder.int_as_varint 2 encoder
  | Map_tile_types.App_icon -> Pbrt.Encoder.int_as_varint 3 encoder
  | Map_tile_types.Avatar -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_image (v:Map_tile_types.image) encoder = 
  begin match v.Map_tile_types.url with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.mime_type with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.width with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.height with
  | Some x -> 
    Pbrt.Encoder.key (4, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.title with
  | Some x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.href with
  | Some x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.x_offset with
  | Some x -> 
    Pbrt.Encoder.key (6, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.y_offset with
  | Some x -> 
    Pbrt.Encoder.key (7, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.use_context with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
    encode_image_use_context x encoder;
  | None -> ();
  end;
  ()

let rec encode_tag (v:Map_tile_types.tag) encoder = 
  begin match v.Map_tile_types.name with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.uri with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  ()

let rec encode_media_file_media_type (v:Map_tile_types.media_file_media_type) encoder =
  match v with
  | Map_tile_types.Audio -> Pbrt.Encoder.int_as_varint 1 encoder
  | Map_tile_types.Video -> Pbrt.Encoder.int_as_varint 2 encoder
  | Map_tile_types.Other -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_media_file (v:Map_tile_types.media_file) encoder = 
  begin match v.Map_tile_types.url with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.mime_type with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.media_type with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
    encode_media_file_media_type x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.length with
  | Some x -> 
    Pbrt.Encoder.key (4, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  ()

let rec encode_content_location (v:Map_tile_types.content_location) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (8, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  ) v.Map_tile_types.weights;
  begin match v.Map_tile_types.loc_x with
  | Some x -> 
    Pbrt.Encoder.key (9, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.loc_y with
  | Some x -> 
    Pbrt.Encoder.key (10, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  ()

let rec encode_category (v:Map_tile_types.category) encoder = 
  begin match v.Map_tile_types.name with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_category x) encoder;
  ) v.Map_tile_types.children;
  ()

let rec encode_owner (v:Map_tile_types.owner) encoder = 
  begin match v.Map_tile_types.name with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.email with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  ()

let rec encode_podcast_episode_episode_type (v:Map_tile_types.podcast_episode_episode_type) encoder =
  match v with
  | Map_tile_types.Full -> Pbrt.Encoder.int_as_varint 1 encoder
  | Map_tile_types.Trailer -> Pbrt.Encoder.int_as_varint 2 encoder
  | Map_tile_types.Bonus -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_podcast_episode (v:Map_tile_types.podcast_episode) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Map_tile_types.id encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Map_tile_types.titles;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Map_tile_types.descriptions;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_image x) encoder;
  ) v.Map_tile_types.images;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_media_file x) encoder;
  ) v.Map_tile_types.media_files;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tag x) encoder;
  ) v.Map_tile_types.tags;
  begin match v.Map_tile_types.location with
  | Some x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_content_location x) encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.pub_time with
  | Some x -> 
    Pbrt.Encoder.key (8, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.publisher_guid with
  | Some x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Map_tile_types.webpage_urls;
  begin match v.Map_tile_types.duration with
  | Some x -> 
    Pbrt.Encoder.key (11, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.explicit with
  | Some x -> 
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.subtitle with
  | Some x -> 
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.episode_number with
  | Some x -> 
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.episode_type with
  | Some x -> 
    Pbrt.Encoder.key (15, Pbrt.Varint) encoder; 
    encode_podcast_episode_episode_type x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.author with
  | Some x -> 
    Pbrt.Encoder.key (16, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  ()

let rec encode_podcast (v:Map_tile_types.podcast) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Map_tile_types.id encoder;
  begin match v.Map_tile_types.title with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.slug with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  ) v.Map_tile_types.descriptions;
  begin match v.Map_tile_types.homepage_url with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_image x) encoder;
  ) v.Map_tile_types.images;
  List.iter (fun x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_podcast_episode x) encoder;
  ) v.Map_tile_types.episodes;
  begin match v.Map_tile_types.location with
  | Some x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_content_location x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tag x) encoder;
  ) v.Map_tile_types.tags;
  begin match v.Map_tile_types.pub_date with
  | Some x -> 
    Pbrt.Encoder.key (10, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.last_build_date with
  | Some x -> 
    Pbrt.Encoder.key (11, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.generator with
  | Some x -> 
    Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.language with
  | Some x -> 
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.copyright with
  | Some x -> 
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.editor_contact with
  | Some x -> 
    Pbrt.Encoder.key (15, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.summary with
  | Some x -> 
    Pbrt.Encoder.key (16, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.author with
  | Some x -> 
    Pbrt.Encoder.key (17, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (18, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_category x) encoder;
  ) v.Map_tile_types.categories;
  begin match v.Map_tile_types.explicit with
  | Some x -> 
    Pbrt.Encoder.key (19, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.owner with
  | Some x -> 
    Pbrt.Encoder.key (20, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_owner x) encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.is_serial with
  | Some x -> 
    Pbrt.Encoder.key (21, Pbrt.Varint) encoder; 
    Pbrt.Encoder.bool x encoder;
  | None -> ();
  end;
  ()

let rec encode_term (v:Map_tile_types.term) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Map_tile_types.value encoder;
  begin match v.Map_tile_types.score with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.x with
  | Some x -> 
    Pbrt.Encoder.key (3, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  begin match v.Map_tile_types.y with
  | Some x -> 
    Pbrt.Encoder.key (4, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | None -> ();
  end;
  ()

let rec encode_cluster_seed (v:Map_tile_types.cluster_seed) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.x encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.y encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Map_tile_types.label encoder;
  ()

let rec encode_delta (v:Map_tile_types.delta) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.dx encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.dy encoder;
  ()

let rec encode_path (v:Map_tile_types.path) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.start_x encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.start_y encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_delta x) encoder;
  ) v.Map_tile_types.deltas;
  ()

let rec encode_map_tile (v:Map_tile_types.map_tile) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.min_x encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.max_x encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.min_y encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Map_tile_types.max_y encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_podcast x) encoder;
  ) v.Map_tile_types.podcasts;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_term x) encoder;
  ) v.Map_tile_types.terms;
  List.iter (fun x -> 
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_path x) encoder;
  ) v.Map_tile_types.paths;
  List.iter (fun x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_cluster_seed x) encoder;
  ) v.Map_tile_types.seeds;
  ()
