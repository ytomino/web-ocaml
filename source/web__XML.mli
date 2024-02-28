val xml_declaration: (string -> int -> int -> unit) -> ?encoding:string ->
	?endline:bool -> unit -> unit

type text_context

val open_text: (string -> int -> int -> unit) -> text_context
val close_text: text_context -> unit
val text_output_substring: text_context -> string -> int -> int -> unit
val text_output_string: text_context -> string -> unit
