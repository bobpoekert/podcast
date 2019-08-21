(** map_tile.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_image_use_context : unit -> image_use_context
(** [default_image_use_context ()] is the default value for type [image_use_context] *)

val default_image : 
  ?url:string option ->
  ?mime_type:string option ->
  ?width:float option ->
  ?height:float option ->
  ?title:string option ->
  ?x_offset:float option ->
  ?y_offset:float option ->
  ?use_context:image_use_context option ->
  unit ->
  image
(** [default_image ()] is the default value for type [image] *)

val default_tag : 
  ?name:string option ->
  ?uri:string option ->
  unit ->
  tag
(** [default_tag ()] is the default value for type [tag] *)

val default_media_file_media_type : unit -> media_file_media_type
(** [default_media_file_media_type ()] is the default value for type [media_file_media_type] *)

val default_media_file : 
  ?url:string option ->
  ?mime_type:string option ->
  ?media_type:media_file_media_type option ->
  ?length:float option ->
  unit ->
  media_file
(** [default_media_file ()] is the default value for type [media_file] *)

val default_content_location : 
  ?weights:float list ->
  ?loc_x:float option ->
  ?loc_y:float option ->
  unit ->
  content_location
(** [default_content_location ()] is the default value for type [content_location] *)

val default_podcast_episode : 
  ?id:int64 ->
  ?title:string option ->
  ?description:string option ->
  ?images:image list ->
  ?media_files:media_file list ->
  ?tags:tag list ->
  ?location:content_location option ->
  ?pub_time:int64 option ->
  unit ->
  podcast_episode
(** [default_podcast_episode ()] is the default value for type [podcast_episode] *)

val default_podcast : 
  ?id:int64 ->
  ?title:string option ->
  ?slug:string option ->
  ?description:string option ->
  ?homepage_url:string option ->
  ?images:image list ->
  ?episodes:podcast_episode list ->
  ?location:content_location option ->
  ?tags:tag list ->
  unit ->
  podcast
(** [default_podcast ()] is the default value for type [podcast] *)

val default_term : 
  ?value:string ->
  ?score:float option ->
  ?x:float option ->
  ?y:float option ->
  unit ->
  term
(** [default_term ()] is the default value for type [term] *)

val default_cluster_seed : 
  ?x:float ->
  ?y:float ->
  ?label:int32 ->
  unit ->
  cluster_seed
(** [default_cluster_seed ()] is the default value for type [cluster_seed] *)

val default_delta : 
  ?dx:float ->
  ?dy:float ->
  unit ->
  delta
(** [default_delta ()] is the default value for type [delta] *)

val default_path : 
  ?start_x:float ->
  ?start_y:float ->
  ?deltas:delta list ->
  unit ->
  path
(** [default_path ()] is the default value for type [path] *)

val default_map_tile : 
  ?min_x:float ->
  ?max_x:float ->
  ?min_y:float ->
  ?max_y:float ->
  ?podcasts:podcast list ->
  ?terms:term list ->
  ?paths:path list ->
  ?seeds:cluster_seed list ->
  unit ->
  map_tile
(** [default_map_tile ()] is the default value for type [map_tile] *)
