type header_row 
type header 
type warc_entry 
type warc_page 

val next_entry : in_channel -> warc_entry
val next_page : in_channel -> warc_page
val iter_pages : in_channel -> (warc_page -> unit) -> unit
val get_url : header -> string
val get_req : warc_page -> warc_entry
val get_rsp : warc_page -> warc_entry
val get_headers : warc_entry -> header
val get_header : header -> string -> string
val get_body : warc_entry -> string
val parse_response_body : string -> (Cohttp.Response.t * string)
val parse_request_body : string -> Cohttp.Request.t