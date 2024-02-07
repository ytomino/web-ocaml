type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

let bool_of_checkbox (value: string): bool = (
	String.length value = 2
	&& Char.lowercase_ascii value.[0] = 'o'
	&& Char.lowercase_ascii value.[1] = 'n'
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

let text_output_string (context: text_context) (s: string) = (
	let {Text.version; print_substring} = context in
	let print_string = make_print_string print_substring in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| '&' -> print_string "&amp;"
		| '<' -> print_string "&lt;"
		| '>' -> print_string "&gt;"
		| ' ' -> print_string "&#32;"
		| '\n' -> print_string (br version)
		| '\r' -> ()
		| _ ->
			print_substring s i 1
		end
	done
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

let attribute_output_string (context: attribute_context) (s: string) = (
	let {Attribute.version; print_substring} = context in
	let print_string = make_print_string print_substring in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| '&' -> print_string "&amp;"
		| '<' -> print_string "&lt;"
		| '>' -> print_string "&gt;"
		| ' ' -> print_string "&#32;"
		| '\'' -> print_string (apos version)
		| '\"' -> print_string "&quot;"
		| '\n' -> print_string "&#10;"
		| '\r' -> ()
		| _ ->
			print_substring s i 1
		end
	done
);;
