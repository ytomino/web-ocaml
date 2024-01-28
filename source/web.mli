module StringMap: Map.S with type key = string

val content_type_multipart_form_data: string
val content_type_url_encoded: string
val content_type_text: string
val content_type_html: string
val content_type_xml: string

type post_encoded = [`unknown | `url_encoded | `multipart_form_data]

val decode_content_type: string -> post_encoded
val decode_cookie: string -> string StringMap.t
val decode_query_string: string -> string StringMap.t
val decode_multipart_form_data: string -> string StringMap.t
val encode_uri: (string -> unit) -> string -> unit

val header_see_other: (string -> unit) -> string -> unit
val header_service_unavailable: (string -> unit) -> unit
val header_content_type: (string -> unit) -> string -> unit
val header_cookie: (string -> unit) -> ?expires:float -> string StringMap.t -> unit
val header_break: (string -> unit) -> unit

module CGI = Web__CGI
module HTML = Web__HTML
