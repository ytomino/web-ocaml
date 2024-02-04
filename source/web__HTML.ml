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

let text_output_string (version: version) (print_string: string -> unit)
	(s: string) =
(
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

let attribute_output_string (version: version) (print_string: string -> unit)
	(s: string) =
(
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
