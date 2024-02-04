type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

val bool_of_checkbox: string -> bool

type text_context

val open_text: version -> (string -> unit) -> text_context
val close_text: text_context -> unit
val text_output_string: text_context -> string -> unit

type attribute_context

val open_attribute: version -> (string -> unit) -> string -> attribute_context
val close_attribute: attribute_context -> unit
val attribute_output_string: attribute_context -> string -> unit
