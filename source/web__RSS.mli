type version = [`rss2]

type 'a context

module type Text = sig
	type element_context
	
	val open_element: version -> (string -> int -> int -> unit) -> element_context
	val close_element: element_context -> unit
	val element_output_substring: element_context -> string -> int -> int -> unit
	val element_output_string: element_context -> string -> unit
end

module Description: sig
	type description_context
	
	include Text with type element_context := description_context
	
	val open_element_at: [< `channel | `item] context -> description_context
	val output_substring: [< `channel | `item] context -> string -> int -> int ->
		unit
	val output_string: [< `channel | `item] context -> string -> unit
end

module Link: sig
	type link_context
	
	include Text with type element_context := link_context
	
	val open_element_at: [< `channel | `item] context -> link_context
	val output_substring: [< `channel | `item] context -> string -> int -> int ->
		unit
	val output_string: [< `channel | `item] context -> string -> unit
end

module Title: sig
	type title_context
	
	include Text with type element_context := title_context
	
	val open_element_at: [< `channel | `item] context -> title_context
	val output_substring: [< `channel | `item] context -> string -> int -> int ->
		unit
	val output_string: [< `channel | `item] context -> string -> unit
end

module Item: sig
	type item_context = [`item] context
	
	val open_element: version -> (string -> int -> int -> unit) -> item_context
	val open_element_at: [`channel] context -> item_context
	val close_element: item_context -> unit
end (** At least one of <title> of <description> is required. *)

module Channel: sig
	type channel_context = [`channel] context
	
	val open_element: version -> (string -> int -> int -> unit) -> channel_context
	val open_element_at: [`rss] context -> channel_context
	val close_element: channel_context -> unit
end (** <title>, <link>, and <description> are required. *)

type rss_context = [`rss] context

val open_element: version -> (string -> int -> int -> unit) -> rss_context
val close_element: rss_context -> unit
