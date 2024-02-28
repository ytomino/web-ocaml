let make_print_string (print_substring: string -> int -> int -> unit)
	(s: string) =
(
	print_substring s 0 (String.length s)
);;

let xml_declaration (print_substring: string -> int -> int -> unit)
	?(encoding: string option) ?(endline: bool = false) () =
(
	let print_string = make_print_string print_substring in
	print_string "<?xml version=\"1.0\"";
	begin match encoding with
	| None ->
		()
	| Some encoding ->
		let ac = Web__HTML.open_attribute `xml print_substring "encoding" in
		Web__HTML.attribute_output_string ac encoding;
		Web__HTML.close_attribute ac
	end;
	print_substring "?>\n" 0 (if endline then 3 else 2)
);;
