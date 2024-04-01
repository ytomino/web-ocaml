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

let nbsp (version: version) = (
	match version with
	| `html4 | `html5 -> "&nbsp;"
	| `xhtml1 | `xhtml5 | `xml -> "&#160;"
);;

module Text = struct
	type text_context = {
		version: version;
		space: [`nbsp | `nbsp_boundary] option;
		newline: [`br] option;
		print_substring: string -> int -> int -> unit;
		mutable state: [`initial | `non_blank | `blank | `nbsp | `cr];
	};;
end;;

type text_context = Text.text_context;;

let print_newline (context: text_context) = (
	let {Text.version; newline; print_substring; _} = context in
	let print_string = make_print_string print_substring in
	print_string (
		match newline with
		| None ->
			"\n" (* substitute "\r" to "\n" *)
		| Some `br ->
			begin match version with
			| `html4 | `html5 ->
				"<br>"
			| `xhtml1 | `xhtml5 | `xml ->
				"<br />"
			end
	)
);;

let open_text (version: version) ?(space: [`nbsp | `nbsp_boundary] option)
	?(newline: [`br] option) (print_substring: string -> int -> int -> unit) =
(
	{Text.version; space; newline; print_substring; state = `initial}
);;

let close_text (context: text_context) = (
	let {Text.version; print_substring; state; _} = context in
	let print_string = make_print_string print_substring in
	match state with
	| `initial ->
		()
	| `non_blank | `nbsp ->
		context.Text.state <- `initial
	| `blank ->
		assert (context.Text.space = Some `nbsp_boundary);
		print_string (nbsp version);
		context.Text.state <- `initial
	| `cr ->
		print_newline context;
		context.Text.state <- `initial
);;

let unsafe_text_output_substring: text_context -> string -> int -> int ->
	unit =
	let to_non_blank context = (
		let {Text.print_substring; state; _} = context in
		let print_string = make_print_string print_substring in
		match state with
		| `initial | `nbsp ->
			context.Text.state <- `non_blank
		| `non_blank ->
			()
		| `blank ->
			print_string " ";
			context.Text.state <- `non_blank
		| `cr ->
			print_newline context;
			context.Text.state <- `non_blank
	) in
	let rec loop context s start i end_pos = (
		let {Text.version; space; print_substring; state; _} = context in
		assert (start = i || state = `non_blank);
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		if i >= end_pos then print_range s start i else
		match s.[i] with
		| '&' ->
			substitute context s start i end_pos "&amp;"
		| '<' ->
			substitute context s start i end_pos "&lt;"
		| '>' ->
			substitute context s start i end_pos "&gt;"
		| ' ' ->
			begin match space with
			| None ->
				others context s start i end_pos
			| Some `nbsp ->
				substitute context s start i end_pos (nbsp version)
			| Some `nbsp_boundary ->
				begin match state with
				| `initial ->
					print_string (nbsp version);
					context.Text.state <- `nbsp;
				| `non_blank ->
					print_range s start i;
					context.Text.state <- `blank;
				| `blank ->
					let nbsp = nbsp version in
					print_string nbsp;
					print_string nbsp;
					context.Text.state <- `nbsp
				| `nbsp ->
					print_string (nbsp version)
				| `cr ->
					print_newline context;
					print_string (nbsp version);
					context.Text.state <- `nbsp;
				end;
				let next = i + 1 in
				loop context s next next end_pos
			end
		| '\n' ->
			begin match state with
			| `initial ->
				()
			| `non_blank ->
				print_range s start i
			| `blank ->
				print_string (nbsp version);
				context.Text.state <- `initial;
			| `nbsp | `cr ->
				context.Text.state <- `initial;
			end;
			print_newline context;
			let next = i + 1 in
			loop context s next next end_pos
		| '\r' ->
			begin match state with
			| `initial | `nbsp ->
				context.Text.state <- `cr
			| `non_blank ->
				print_range s start i;
				context.Text.state <- `cr
			| `blank ->
				print_string (nbsp version);
				context.Text.state <- `cr
			| `cr ->
				print_newline context
			end;
			let next = i + 1 in
			loop context s next next end_pos
		| _ ->
			others context s start i end_pos
	) and substitute context s start i end_pos cer = (
		let {Text.print_substring; _} = context in
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		if start < i then print_range s start i
		else to_non_blank context;
		print_string cer;
		let next = i + 1 in
		loop context s next next end_pos
	) and others context s start i end_pos = (
		to_non_blank context;
		loop context s start (i + 1) end_pos
	) in
	fun context s pos len -> loop context s pos pos (pos + len);;

let text_output_substring (context: text_context) (s: string) (pos: int)
	(len: int) =
(
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_text_output_substring context s pos len
	else invalid_arg "Web.HTML.text_output_substring" (* __FUNCTION__ *)
);;

let text_output_string (context: text_context) (s: string) = (
	make_print_string (unsafe_text_output_substring context) s
);;

let output_closing_empty_element (version: version)
	(print_substring: string -> int -> int -> unit) ?(endline: bool = false) () =
(
	let startpos =
		match version with
		| `html4 | `html5 -> 2
		| `xhtml1 | `xhtml5 | `xml -> 0
	in
	let endpos = if endline then 4 else 3 in
	print_substring " />\n" startpos (endpos - startpos)
);;

let newline (version: version) = (
	match version with
	| `html4 -> "\n" (* for very old browser *)
	| `html5 -> "&NewLine;"
	| `xhtml1 | `xhtml5 | `xml -> "&#10;"
);;

let string_of_cr (version: version) = (
	match version with
	| `html4 -> "\n" (* for very old browser *)
	| `html5 | `xhtml1 | `xhtml5 | `xml -> "&#13;"
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
		print_substring: string -> int -> int -> unit;
		mutable cr: bool
	};;
end;;

type attribute_context = Attribute.attribute_context;;

let flush_cr (context: attribute_context) = (
	let {Attribute.version; print_substring; cr} = context in
	let print_string = make_print_string print_substring in
	if cr then (
		print_string (string_of_cr version);
		context.Attribute.cr <- false
	)
);;

let open_attribute (version: version)
	(print_substring: string -> int -> int -> unit) (name: string) =
(
	let print_string = make_print_string print_substring in
	print_string " ";
	print_string name;
	print_string "=\"";
	{Attribute.version; print_substring; cr = false}
);;

let close_attribute (context: attribute_context) = (
	flush_cr context;
	let {Attribute.print_substring; _} = context in
	let print_string = make_print_string print_substring in
	print_string "\""
);;

let unsafe_attribute_output_substring: attribute_context -> string -> int ->
	int -> unit =
	let rec loop context s start i end_pos = (
		let {Attribute.version; print_substring; cr} = context in
		assert (start = i || not cr);
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		if i >= end_pos then print_range s start i else
		match s.[i] with
		| '&' ->
			substitute context s start i end_pos "&amp;"
		| '<' ->
			substitute context s start i end_pos "&lt;"
		| '>' ->
			substitute context s start i end_pos "&gt;"
		| '\'' ->
			substitute context s start i end_pos (apos version)
		| '\"' ->
			substitute context s start i end_pos "&quot;"
		| '\n' ->
			if start < i then print_range s start i else
			if cr then (
				(* substitute "\r" or "\r\n" to "\n" for HTML4 *)
				begin match version with
				| `html4 -> ()
				| `html5 | `xhtml1 | `xhtml5 | `xml -> print_string (string_of_cr version)
				end;
				context.Attribute.cr <- false
			);
			print_string (newline version);
			let next = i + 1 in
			loop context s next next end_pos
		| '\r' ->
			if not cr then (
				print_range s start i;
				context.Attribute.cr <- true
			) else print_string (string_of_cr version);
			let next = i + 1 in
			loop context s next next end_pos
		| _ ->
			flush_cr context;
			loop context s start (i + 1) end_pos
	) and substitute context s start i end_pos cer = (
		let {Attribute.print_substring; _} = context in
		let print_range = make_print_range print_substring in
		let print_string = make_print_string print_substring in
		if start < i then print_range s start i
		else flush_cr context;
		print_string cer;
		let next = i + 1 in
		loop context s next next end_pos
	) in
	fun context s pos len -> loop context s pos pos (pos + len);;

let attribute_output_substring (context: attribute_context) (s: string)
	(pos: int) (len: int) =
(
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_attribute_output_substring context s pos len
	else invalid_arg "Web.HTML.attribute_output_substring" (* __FUNCTION__ *)
);;

let attribute_output_string (context: attribute_context) (s: string) = (
	make_print_string (unsafe_attribute_output_substring context) s
);;

module Input = Web__HTML__Input;;
