[@@@ocaml.warning "-27-30-39"]


type image_use_context =
  | Thumbnail 
  | Cover 
  | Favicon 
  | App_icon 
  | Avatar 

type image = {
  url : string option;
  mime_type : string option;
  width : float option;
  height : float option;
  title : string option;
  x_offset : float option;
  y_offset : float option;
  use_context : image_use_context option;
}

type tag = {
  name : string option;
  uri : string option;
}

type media_file_media_type =
  | Audio 
  | Video 
  | Other 

type media_file = {
  url : string option;
  mime_type : string option;
  media_type : media_file_media_type option;
  length : float option;
}

type content_location = {
  weights : float list;
  loc_x : float option;
  loc_y : float option;
}

type podcast_episode = {
  id : int64;
  title : string option;
  description : string option;
  images : image list;
  media_files : media_file list;
  tags : tag list;
  location : content_location option;
  pub_time : int64 option;
}

type podcast = {
  id : int64;
  title : string option;
  slug : string option;
  description : string option;
  homepage_url : string option;
  images : image list;
  episodes : podcast_episode list;
  location : content_location option;
  tags : tag list;
}

type term = {
  value : string;
  score : float option;
  x : float option;
  y : float option;
}

type cluster_seed = {
  x : float;
  y : float;
  label : int32;
}

type delta = {
  dx : float;
  dy : float;
}

type path = {
  start_x : float;
  start_y : float;
  deltas : delta list;
}

type map_tile = {
  min_x : float;
  max_x : float;
  min_y : float;
  max_y : float;
  podcasts : podcast list;
  terms : term list;
  paths : path list;
  seeds : cluster_seed list;
}

let rec default_image_use_context () = (Thumbnail:image_use_context)

let rec default_image 
  ?url:((url:string option) = None)
  ?mime_type:((mime_type:string option) = None)
  ?width:((width:float option) = None)
  ?height:((height:float option) = None)
  ?title:((title:string option) = None)
  ?x_offset:((x_offset:float option) = None)
  ?y_offset:((y_offset:float option) = None)
  ?use_context:((use_context:image_use_context option) = None)
  () : image  = {
  url;
  mime_type;
  width;
  height;
  title;
  x_offset;
  y_offset;
  use_context;
}

let rec default_tag 
  ?name:((name:string option) = None)
  ?uri:((uri:string option) = None)
  () : tag  = {
  name;
  uri;
}

let rec default_media_file_media_type () = (Audio:media_file_media_type)

let rec default_media_file 
  ?url:((url:string option) = None)
  ?mime_type:((mime_type:string option) = None)
  ?media_type:((media_type:media_file_media_type option) = None)
  ?length:((length:float option) = None)
  () : media_file  = {
  url;
  mime_type;
  media_type;
  length;
}

let rec default_content_location 
  ?weights:((weights:float list) = [])
  ?loc_x:((loc_x:float option) = None)
  ?loc_y:((loc_y:float option) = None)
  () : content_location  = {
  weights;
  loc_x;
  loc_y;
}

let rec default_podcast_episode 
  ?id:((id:int64) = 0L)
  ?title:((title:string option) = None)
  ?description:((description:string option) = None)
  ?images:((images:image list) = [])
  ?media_files:((media_files:media_file list) = [])
  ?tags:((tags:tag list) = [])
  ?location:((location:content_location option) = None)
  ?pub_time:((pub_time:int64 option) = None)
  () : podcast_episode  = {
  id;
  title;
  description;
  images;
  media_files;
  tags;
  location;
  pub_time;
}

let rec default_podcast 
  ?id:((id:int64) = 0L)
  ?title:((title:string option) = None)
  ?slug:((slug:string option) = None)
  ?description:((description:string option) = None)
  ?homepage_url:((homepage_url:string option) = None)
  ?images:((images:image list) = [])
  ?episodes:((episodes:podcast_episode list) = [])
  ?location:((location:content_location option) = None)
  ?tags:((tags:tag list) = [])
  () : podcast  = {
  id;
  title;
  slug;
  description;
  homepage_url;
  images;
  episodes;
  location;
  tags;
}

let rec default_term 
  ?value:((value:string) = "")
  ?score:((score:float option) = None)
  ?x:((x:float option) = None)
  ?y:((y:float option) = None)
  () : term  = {
  value;
  score;
  x;
  y;
}

let rec default_cluster_seed 
  ?x:((x:float) = 0.)
  ?y:((y:float) = 0.)
  ?label:((label:int32) = 0l)
  () : cluster_seed  = {
  x;
  y;
  label;
}

let rec default_delta 
  ?dx:((dx:float) = 0.)
  ?dy:((dy:float) = 0.)
  () : delta  = {
  dx;
  dy;
}

let rec default_path 
  ?start_x:((start_x:float) = 0.)
  ?start_y:((start_y:float) = 0.)
  ?deltas:((deltas:delta list) = [])
  () : path  = {
  start_x;
  start_y;
  deltas;
}

let rec default_map_tile 
  ?min_x:((min_x:float) = 0.)
  ?max_x:((max_x:float) = 0.)
  ?min_y:((min_y:float) = 0.)
  ?max_y:((max_y:float) = 0.)
  ?podcasts:((podcasts:podcast list) = [])
  ?terms:((terms:term list) = [])
  ?paths:((paths:path list) = [])
  ?seeds:((seeds:cluster_seed list) = [])
  () : map_tile  = {
  min_x;
  max_x;
  min_y;
  max_y;
  podcasts;
  terms;
  paths;
  seeds;
}
