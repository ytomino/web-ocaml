open Web

val request_uri: unit -> string

val post: unit -> bool
val post_encoded: unit -> post_encoded
val post_length: unit -> int
val read_input: unit -> string StringMap.t

val remote_addr: unit -> string
val remote_host: unit -> string
