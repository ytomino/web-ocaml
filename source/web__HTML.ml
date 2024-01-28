let bool_of_checkbox (value: string): bool = (
	String.length value = 2
	&& Char.lowercase_ascii value.[0] = 'o'
	&& Char.lowercase_ascii value.[1] = 'n'
);;

let encode_html ~(xhtml: bool) (print_string: string -> unit) (s: string) = (
	let buf1 = Bytes.make 1 ' ' in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| '&' -> print_string "&amp;"
		| '<' -> print_string "&lt;"
		| '>' -> print_string "&gt;"
		| ' ' -> print_string "&#32;"
		| '\n' -> print_string (if xhtml then "<br />" else "<br>")
		| '\r' -> ()
		| _ as c ->
			Bytes.set buf1 0 c;
			print_string (Bytes.unsafe_to_string buf1)
		end
	done
);;

let encode_entity (print_string: string -> unit) (s: string) = (
	let buf1 = Bytes.make 1 ' ' in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| '&' -> print_string "&amp;"
		| '<' -> print_string "&lt;"
		| '>' -> print_string "&gt;"
		| ' ' -> print_string "&#32;"
		| '\'' -> print_string "&apos;"
		| '\"' -> print_string "&quot;"
		| '\n' -> print_string "&#10;"
		| '\r' -> ()
		| _ as c ->
			Bytes.set buf1 0 c;
			print_string (Bytes.unsafe_to_string buf1)
		end
	done
);;
