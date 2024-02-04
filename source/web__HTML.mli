type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

val bool_of_checkbox: string -> bool

type text_context

val open_text: version -> (string -> unit) -> text_context
val close_text: text_context -> unit
val text_output_string: text_context -> string -> unit

val attribute_output_string: version -> (string -> unit) -> string -> unit
