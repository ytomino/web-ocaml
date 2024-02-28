open Web;;
open Web__HTML;;

let make_print_string (print_substring: string -> int -> int -> unit)
	(s: string) =
(
	print_substring s 0 (String.length s)
);;

let output_map (version: version)
	(print_substring: string -> int -> int -> unit) ?(endline: bool option)
	(_: [`hidden]) (m: string StringMap.t) =
(
	let print_string = make_print_string print_substring in
	StringMap.iter (fun key value ->
		print_string "<input type=\"hidden\"";
		let ac = open_attribute version print_substring "name" in
		attribute_output_string ac key;
		close_attribute ac;
		let ac = open_attribute version print_substring "value" in
		attribute_output_string ac value;
		close_attribute ac;
		output_closing_empty_element version print_substring ?endline ()
	) m
);;
