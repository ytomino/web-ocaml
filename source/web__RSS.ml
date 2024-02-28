type version = [`rss2];;

let make_print_string (print_substring: string -> int -> int -> unit)
	(s: string) =
(
	print_substring s 0 (String.length s)
);;

type 'a context = {
	version: version;
	print_substring: string -> int -> int -> unit
};;

let make_open_element (endline: bool) (tag: string) (version: version)
	(print_substring: string -> int -> int -> unit) =
(
	let print_string = make_print_string print_substring in
	print_string "<";
	print_string tag;
	print_substring ">\n" 0 (if endline then 2 else 1);
	{version; print_substring}
);;

let make_close_element (type a) (tag: string) (context: a context) = (
	let {print_substring; _} = context in
	let print_string = make_print_string print_substring in
	print_string "</";
	print_string tag;
	print_string ">\n"
);;

module type Text = sig
	type element_context
	
	val open_element: version -> (string -> int -> int -> unit) -> element_context
	val close_element: element_context -> unit
	val element_output_substring: element_context -> string -> int -> int -> unit
	val element_output_string: element_context -> string -> unit
end;;

module Text
	(Param: sig
		val tag: string
	end) =
struct
	type element_context = unit context * Web__XML.text_context;;
	
	let open_element (version: version)
		(print_substring: string -> int -> int -> unit) =
	(
		make_open_element false Param.tag version print_substring,
		Web__XML.open_text print_substring
	);;
	
	let open_element_at (type a) (parent_context: a context) = (
		let {version; print_substring} = parent_context in
		open_element version print_substring
	);;
	
	let close_element (context: element_context) = (
		let parent_context, text_context = context in
		Web__XML.close_text text_context;
		make_close_element Param.tag parent_context
	);;
	
	let element_output_substring (context: element_context) (s: string) (pos: int)
		(len: int) =
	(
		let _, text_context = context in
		Web__XML.text_output_substring text_context s pos len
	);;
	
	let element_output_string (context: element_context) (s: string) = (
		let _, text_context = context in
		Web__XML.text_output_string text_context s
	);;
end;;

module Description = struct
	include Text
		(struct
			let tag = "description"
		end);;
	
	type description_context = element_context;;
end;;

module Link = struct
	include Text
		(struct
			let tag = "link"
		end);;
	
	type link_context = element_context;;
end;;

module Title = struct
	include Text
		(struct
			let tag = "title"
		end);;
	
	type title_context = element_context;;
end;;

module Item = struct
	type item_context = [`item] context
	
	let open_element: version -> (string -> int -> int -> unit) -> item_context =
		make_open_element true "item";;
	
	let open_element_at (parent_context: [`channel] context) = (
		let {version; print_substring} = parent_context in
		open_element version print_substring
	);;
	
	let close_element: item_context -> unit = make_close_element "item";;
end

module Channel = struct
	type channel_context = [`channel] context
	
	let open_element: version -> (string -> int -> int -> unit) ->
		channel_context =
		make_open_element true "channel";;
	
	let open_element_at (parent_context: [`rss] context) = (
		let {version; print_substring} = parent_context in
		open_element version print_substring
	);;
	
	let close_element: channel_context -> unit = make_close_element "channel";;
end

type rss_context = [`rss] context;;

let open_element (version: version)
	(print_substring: string -> int -> int -> unit) =
(
	let print_string = make_print_string print_substring in
	print_string "<rss version=\"2.0\">\n";
	{version; print_substring}
);;

let close_element: rss_context -> unit = make_close_element "rss";;
