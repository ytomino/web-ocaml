open Web

val request_scheme: unit -> string
val host: unit -> string
val request_uri: unit -> string
val request_path: unit -> string
val query: unit -> string StringMap.t
val cookie: unit -> string StringMap.t

val post: unit -> bool
val post_encoded: unit -> post_encoded
val post_length: unit -> int
val read_post: unit -> string StringMap.t

val remote_addr: unit -> string
val remote_host: unit -> string
val user_agent: unit -> string
