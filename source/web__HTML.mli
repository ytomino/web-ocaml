type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

val bool_of_checkbox: string -> bool

val text_output_string: version -> (string -> unit) -> string -> unit
val attribute_output_string: version -> (string -> unit) -> string -> unit
