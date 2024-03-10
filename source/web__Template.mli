type parsed_info
type template = private parsed_info * string

val parsed_info_revision: int

exception Parse_failure of int * string

val parse_substring: string -> int -> int -> template
val parse_string: string -> template

val repair: (parsed_info * string) -> template

val is_empty: template -> bool
val kind: template -> [`element | `attribute]

val find_opt: template -> string -> template option

val find: template -> string -> template

exception Unhandled_tag of string

val unhandled: string -> 'a

exception Not_found_tag of string

val output_template_with:
	(string -> 'a -> (string -> int -> int -> unit) -> template -> 'a) ->
	'a -> (string -> int -> int -> unit) -> template -> 'a

val output_subtemplate_with:
	(string -> 'a -> (string -> int -> int -> unit) -> template -> 'a) ->
	'a -> (string -> int -> int -> unit) -> template -> string -> 'a

val output_template: (string -> int -> int -> unit) -> template -> unit

val output_subtemplate: (string -> int -> int -> unit) -> template -> string ->
	unit
