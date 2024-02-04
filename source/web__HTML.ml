type version = [`html4 | `html5 | `xhtml1 | `xhtml5 | `xml]

let bool_of_checkbox (value: string): bool = (
	String.length value = 2
	&& Char.lowercase_ascii value.[0] = 'o'
	&& Char.lowercase_ascii value.[1] = 'n'
);;

let br (version: version) = (
	match version with
	| `html4 | `html5 -> "<br>"
	| `xhtml1 | `xhtml5 | `xml -> "<br />"
);;

module Text = struct
	type text_context = {
		version: version;
		print_string: string -> unit
	};;
end;;

type text_context = Text.text_context;;

let open_text (version: version) (print_string: string -> unit) = (
	{Text.version; print_string}
);;

let close_text (_: text_context) = ();;

let text_output_string (context: text_context) (s: string) = (
	let {Text.version; print_string} = context in
	let buf1 = Bytes.make 1 ' ' in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| '&' -> print_string "&amp;"
		| '<' -> print_string "&lt;"
		| '>' -> print_string "&gt;"
		| ' ' -> print_string "&#32;"
		| '\n' -> print_string (br version)
		| '\r' -> ()
		| _ as c ->
			Bytes.set buf1 0 c;
			print_string (Bytes.unsafe_to_string buf1)
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
		print_string: string -> unit
	};;
end;;

type attribute_context = Attribute.attribute_context;;

let open_attribute (version: version) (print_string: string -> unit)
	(name: string) =
(
	print_string " ";
	print_string name;
	print_string "=\"";
	{Attribute.version; print_string}
);;

let close_attribute (context: attribute_context) = (
	let {Attribute.print_string; _} = context in
	print_string "\""
);;

let attribute_output_string (context: attribute_context) (s: string) = (
	let {Attribute.version; print_string} = context in
	let buf1 = Bytes.make 1 ' ' in
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
		| _ as c ->
			Bytes.set buf1 0 c;
			print_string (Bytes.unsafe_to_string buf1)
		end
	done
);;
