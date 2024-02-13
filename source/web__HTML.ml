type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

let bool_of_checkbox (value: string): bool = (
	String.length value = 2
	&& Char.lowercase_ascii value.[0] = 'o'
	&& Char.lowercase_ascii value.[1] = 'n'
);;

let make_print_range (print_substring: string -> int -> int -> unit)
	(s: string) (startpos: int) (endpos: int) =
(
	if startpos < endpos then print_substring s startpos (endpos - startpos)
);;

let make_print_string (print_substring: string -> int -> int -> unit)
	(s: string) =
(
	print_substring s 0 (String.length s)
);;

let br (version: version) = (
	match version with
	| `html4 | `html5 -> "<br>"
	| `xhtml1 | `xhtml5 | `xml -> "<br />"
);;

module Text = struct
	type text_context = {
		version: version;
		print_substring: string -> int -> int -> unit
	};;
end;;

type text_context = Text.text_context;;

let open_text (version: version)
	(print_substring: string -> int -> int -> unit) =
(
	{Text.version; print_substring}
);;

let close_text (_: text_context) = ();;

let unsafe_text_output_substring: text_context -> string -> int -> int ->
	unit =
	let rec loop context s start i end_pos = (
		let {Text.version; print_substring} = context in
		let print_range = make_print_range print_substring in
		if i >= end_pos then print_range s start i else
		match s.[i] with
		| '&' ->
			substitute context s start i end_pos "&amp;"
		| '<' ->
			substitute context s start i end_pos "&lt;"
		| '>' ->
			substitute context s start i end_pos "&gt;"
		| ' ' ->
			substitute context s start i end_pos "&#32;"
		| '\n' ->
			substitute context s start i end_pos (br version)
		| '\r' ->
			print_range s start i;
			let next = i + 1 in
			loop context s next next end_pos
		| _ ->
			loop context s start (i + 1) end_pos
	) and substitute context s start i end_pos cer = (
		let {Text.print_substring; _} = context in
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		print_range s start i;
		print_string cer;
		let next = i + 1 in
		loop context s next next end_pos
	) in
	fun context s pos len -> loop context s pos pos (pos + len);;

let text_output_substring (context: text_context) (s: string) (pos: int)
	(len: int) =
(
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_text_output_substring context s pos len
	else invalid_arg "Web.HTML.text_output_substring" (* __FUNCTION__ *)
);;

let text_output_string (context: text_context) (s: string) = (
	make_print_string (unsafe_text_output_substring context) s
);;

let apos (version: version) = (
	match version with
	| `xhtml1 -> "&#39;"
	| `html4 (* "&apos;" is illegal for HTML4, but needed for very old browser *)
	| `html5 | `xhtml5 | `xml -> "&apos;"
);;

module Attribute = struct
	type attribute_context = {
		version: version;
		print_substring: string -> int -> int -> unit
	};;
end;;

type attribute_context = Attribute.attribute_context;;

let open_attribute (version: version)
	(print_substring: string -> int -> int -> unit) (name: string) =
(
	let print_string = make_print_string print_substring in
	print_string " ";
	print_string name;
	print_string "=\"";
	{Attribute.version; print_substring}
);;

let close_attribute (context: attribute_context) = (
	let {Attribute.print_substring; _} = context in
	let print_string = make_print_string print_substring in
	print_string "\""
);;

let unsafe_attribute_output_substring: attribute_context -> string -> int ->
	int -> unit =
	let rec loop context s start i end_pos = (
		let {Attribute.version; print_substring} = context in
		let print_range = make_print_range print_substring in
		if i >= end_pos then print_range s start i else
		match s.[i] with
		| '&' ->
			substitute context s start i end_pos "&amp;"
		| '<' ->
			substitute context s start i end_pos "&lt;"
		| '>' ->
			substitute context s start i end_pos "&gt;"
		| ' ' ->
			substitute context s start i end_pos "&#32;"
		| '\'' ->
			substitute context s start i end_pos (apos version)
		| '\"' ->
			substitute context s start i end_pos "&quot;"
		| '\n' ->
			substitute context s start i end_pos "&#10;"
		| '\r' ->
			print_range s start i;
			let next = i + 1 in
			loop context s next next end_pos
		| _ ->
			loop context s start (i + 1) end_pos
	) and substitute context s start i end_pos cer = (
		let {Attribute.print_substring; _} = context in
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		print_range s start i;
		print_string cer;
		let next = i + 1 in
		loop context s next next end_pos
	) in
	fun context s pos len -> loop context s pos pos (pos + len);;

let attribute_output_substring (context: attribute_context) (s: string)
	(pos: int) (len: int) =
(
	if pos >= 0 && len >= 0 && pos + len <= String.length s
	then unsafe_attribute_output_substring context s pos len
	else invalid_arg "Web.HTML.attribute_output_substring" (* __FUNCTION__ *)
);;

let attribute_output_string (context: attribute_context) (s: string) = (
	make_print_string (unsafe_attribute_output_substring context) s
);;
