type header_row 
type header 
type warc_entry 
type warc_page 

val load_file : string -> Gzip.in_channel
val close_file : Gzip.in_channel -> unit
val next_entry : Gzip.in_channel -> warc_entry
val next_page : Gzip.in_channel -> warc_page option
val iter_pages : Gzip.in_channel -> (warc_page -> unit) -> unit
val get_url : header -> string
val get_req : warc_page -> warc_entry
val get_rsp : warc_page -> warc_entry
val get_header : warc_entry -> header
val get_body : warc_entry -> string
