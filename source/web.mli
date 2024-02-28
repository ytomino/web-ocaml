module StringMap: Map.S with type key = string

val application_x_www_form_urlencoded: string
val application_xhtml_xml: string
val multipart_form_data: string
val text_html: string
val text_plain: string
val text_xml: string

val encode_date: float -> string
val decode_date: string -> float
val encode_uri_path: string -> string
val decode_uri_path: string -> string
val encode_uri_query: string -> string
val decode_uri_query: string -> string
val encode_query_string: string StringMap.t -> string
val decode_query_string: string -> string StringMap.t
val decode_cookie: string -> string StringMap.t

type post_encoded = [`unknown | `urlencoded | `multipart_form_data]

val decode_content_type: string -> post_encoded
val decode_multipart_form_data: string -> string StringMap.t

val header_see_other: (string -> int -> int -> unit) -> string -> unit
val header_service_unavailable: (string -> int -> int -> unit) -> unit -> unit
val header_content_type: (string -> int -> int -> unit) -> string -> unit
val header_cookie: (string -> int -> int -> unit) -> ?expires:float ->
	string StringMap.t -> unit
val header_break: (string -> int -> int -> unit) -> unit -> unit

type query_string_context

val open_query_string: ?append:[`path | `query] ->
	(string -> int -> int -> unit) -> query_string_context
val close_query_string: query_string_context -> unit
val query_string_output_map: query_string_context -> string StringMap.t -> unit

module CGI = Web__CGI
module HTML = Web__HTML
module Locking = Web__Locking
module RSS = Web__RSS
module XML = Web__XML
