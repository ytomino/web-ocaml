type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

val bool_of_checkbox: string -> bool

type text_context

val open_text: version -> ?space:[`nbsp | `nbsp_boundary] ->
	?newline:[`br] -> (string -> int -> int -> unit) -> text_context
val close_text: text_context -> unit
val text_output_substring: text_context -> string -> int -> int -> unit
val text_output_string: text_context -> string -> unit

val output_closing_empty_element: version -> (string -> int -> int -> unit) ->
	unit -> unit

type attribute_context

val open_attribute: version -> (string -> int -> int -> unit) -> string ->
	attribute_context
val close_attribute: attribute_context -> unit
val attribute_output_substring: attribute_context -> string -> int -> int ->
	unit
val attribute_output_string: attribute_context -> string -> unit

module Input = Web__HTML__Input
