(** map_tile.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_image_use_context : Map_tile_types.image_use_context -> Pbrt.Encoder.t -> unit
(** [encode_image_use_context v encoder] encodes [v] with the given [encoder] *)

val encode_image : Map_tile_types.image -> Pbrt.Encoder.t -> unit
(** [encode_image v encoder] encodes [v] with the given [encoder] *)

val encode_tag : Map_tile_types.tag -> Pbrt.Encoder.t -> unit
(** [encode_tag v encoder] encodes [v] with the given [encoder] *)

val encode_media_file_media_type : Map_tile_types.media_file_media_type -> Pbrt.Encoder.t -> unit
(** [encode_media_file_media_type v encoder] encodes [v] with the given [encoder] *)

val encode_media_file : Map_tile_types.media_file -> Pbrt.Encoder.t -> unit
(** [encode_media_file v encoder] encodes [v] with the given [encoder] *)

val encode_content_location : Map_tile_types.content_location -> Pbrt.Encoder.t -> unit
(** [encode_content_location v encoder] encodes [v] with the given [encoder] *)

val encode_podcast_episode : Map_tile_types.podcast_episode -> Pbrt.Encoder.t -> unit
(** [encode_podcast_episode v encoder] encodes [v] with the given [encoder] *)

val encode_podcast : Map_tile_types.podcast -> Pbrt.Encoder.t -> unit
(** [encode_podcast v encoder] encodes [v] with the given [encoder] *)

val encode_term : Map_tile_types.term -> Pbrt.Encoder.t -> unit
(** [encode_term v encoder] encodes [v] with the given [encoder] *)

val encode_cluster_seed : Map_tile_types.cluster_seed -> Pbrt.Encoder.t -> unit
(** [encode_cluster_seed v encoder] encodes [v] with the given [encoder] *)

val encode_delta : Map_tile_types.delta -> Pbrt.Encoder.t -> unit
(** [encode_delta v encoder] encodes [v] with the given [encoder] *)

val encode_path : Map_tile_types.path -> Pbrt.Encoder.t -> unit
(** [encode_path v encoder] encodes [v] with the given [encoder] *)

val encode_map_tile : Map_tile_types.map_tile -> Pbrt.Encoder.t -> unit
(** [encode_map_tile v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_image_use_context : Pbrt.Decoder.t -> Map_tile_types.image_use_context
(** [decode_image_use_context decoder] decodes a [image_use_context] value from [decoder] *)

val decode_image : Pbrt.Decoder.t -> Map_tile_types.image
(** [decode_image decoder] decodes a [image] value from [decoder] *)

val decode_tag : Pbrt.Decoder.t -> Map_tile_types.tag
(** [decode_tag decoder] decodes a [tag] value from [decoder] *)

val decode_media_file_media_type : Pbrt.Decoder.t -> Map_tile_types.media_file_media_type
(** [decode_media_file_media_type decoder] decodes a [media_file_media_type] value from [decoder] *)

val decode_media_file : Pbrt.Decoder.t -> Map_tile_types.media_file
(** [decode_media_file decoder] decodes a [media_file] value from [decoder] *)

val decode_content_location : Pbrt.Decoder.t -> Map_tile_types.content_location
(** [decode_content_location decoder] decodes a [content_location] value from [decoder] *)

val decode_podcast_episode : Pbrt.Decoder.t -> Map_tile_types.podcast_episode
(** [decode_podcast_episode decoder] decodes a [podcast_episode] value from [decoder] *)

val decode_podcast : Pbrt.Decoder.t -> Map_tile_types.podcast
(** [decode_podcast decoder] decodes a [podcast] value from [decoder] *)

val decode_term : Pbrt.Decoder.t -> Map_tile_types.term
(** [decode_term decoder] decodes a [term] value from [decoder] *)

val decode_cluster_seed : Pbrt.Decoder.t -> Map_tile_types.cluster_seed
(** [decode_cluster_seed decoder] decodes a [cluster_seed] value from [decoder] *)

val decode_delta : Pbrt.Decoder.t -> Map_tile_types.delta
(** [decode_delta decoder] decodes a [delta] value from [decoder] *)

val decode_path : Pbrt.Decoder.t -> Map_tile_types.path
(** [decode_path decoder] decodes a [path] value from [decoder] *)

val decode_map_tile : Pbrt.Decoder.t -> Map_tile_types.map_tile
(** [decode_map_tile decoder] decodes a [map_tile] value from [decoder] *)
