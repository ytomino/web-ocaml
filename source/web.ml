module StringMap = Map.Make(String);;

module Private = struct

	let getenv name = (
		try Sys.getenv name with Not_found -> ""
	);;

	let prefixed sub s = (
		let sub_length = String.length sub in
		let s_length = String.length s in
		if sub_length > s_length then (
			false
		) else if sub_length < s_length then (
			sub = String.sub s 0 sub_length
		) else (
			sub = s
		)
	);;

	let string_index_from source i sub = (
		let rec loop i = (
			let p = String.index_from source i sub.[0] in
			if p + String.length sub > String.length source then (
				raise Not_found
			) else if String.sub source p (String.length sub) = sub then (
				p
			) else (
				loop (p + 1)
			)
		) in 
		loop i
	);;

	let is_hex c = (
		begin match c with
		| '0'..'9' | 'A'..'F' | 'a'..'f' -> true
		| _ -> false
		end
	);;

	let int_of_hex c = (
		begin match c with
		| '0'..'9' -> int_of_char c - int_of_char '0'
		| 'A'..'F' -> int_of_char c - (int_of_char 'A' - 10)
		| 'a'..'f' -> int_of_char c - (int_of_char 'a' - 10)
		| _ -> raise (Failure "int_of_hex")
		end
	);;

	let hex_of_int n = (
		"0123456789abcdef".[n]
	);;

	let string_of_weekday wday = (
		begin match wday with
		| 0 -> "Sun"
		| 1 -> "Mon"
		| 2 -> "Tue"
		| 3 -> "Wed"
		| 4 -> "Thu"
		| 5 -> "Fri"
		| 6 -> "Sat"
		| _ -> raise (Failure "string_of_weekday")
		end
	);;

	let string_of_month month = (
		begin match month with
		| 0 -> "Jan"
		| 1 -> "Feb"
		| 2 -> "Mar"
		| 3 -> "Apr"
		| 4 -> "May"
		| 5 -> "Jun"
		| 6 -> "Jul"
		| 7 -> "Aug"
		| 8 -> "Sep"
		| 9 -> "Oct"
		| 10 -> "Nov"
		| 11 -> "Dec"
		| _ -> raise (Failure "string_of_month")
		end
	);;

	let env_request_uri = "REQUEST_URI";;
	let env_query_string = "QUERY_STRING";;
	let env_http_cookie = "HTTP_COOKIE";;
	let env_request_method = "REQUEST_METHOD";;
	let env_content_type = "CONTENT_TYPE";;
	let env_content_length = "CONTENT_LENGTH";;
	let env_remote_addr = "REMOTE_ADDR";;
	let env_remote_host = "REMOTE_HOST";;

	let post = lazy (
		let request_method_value = getenv env_request_method in
		String.lowercase_ascii request_method_value = "post"
	);;
	
	let decode_uri (source: string) = (
		let source_length = String.length source in
		let result = Buffer.create source_length in
		let i = ref 0 in
		while !i < source_length do
			begin match source.[!i] with
			| '+' ->
				Buffer.add_char result ' ';
				incr i
			| '%' ->
				incr i;
				if !i < source_length && is_hex source.[!i] then (
					let hi = int_of_hex source.[!i] in
					incr i;
					if !i < source_length && is_hex source.[!i] then (
						let lo = int_of_hex source.[!i] in
						incr i;
						Buffer.add_char result (char_of_int (hi * 16 + lo))
					) else (
						Buffer.add_char result (char_of_int hi)
					)
				) else (
					Buffer.add_char result '\x00'
				)
			| _ as c ->
				Buffer.add_char result c;
				incr i
			end
		done;
		Buffer.contents result
	);;
	
	let decode_query_string_or_cookie (separator: char) (source: string) = (
		let rec loop source i result = (
			let source_length = String.length source in
			if i < source_length then (
				let next = try String.index_from source i separator with Not_found -> source_length in
				let sub_length = next - i in
				let sub = String.sub source i (next - i) in
				let eq_pos = try String.index sub '=' with Not_found -> sub_length in
				let name = String.sub sub 0 eq_pos in
				let value = decode_uri (String.sub sub (eq_pos + 1) (sub_length - (eq_pos + 1))) in
				loop source (next + 1) (StringMap.add name value result)
			) else (
				result
			)
		) in
		loop source 0 StringMap.empty
	);;

end;;
open Private;;

let request_uri () = (
	let request_uri_value = getenv env_request_uri in
	let query_string_value = getenv env_query_string in
	if query_string_value = "" || String.contains request_uri_value '?' then (
		request_uri_value
	) else (
		request_uri_value ^ "?" ^ query_string_value
	)
);;

let content_type_multipart_form_data = "multipart/form-data";;
let content_type_url_encoded = "application/x-www-form-urlencoded";;
let content_type_text = "text/plain";;
let content_type_html = "text/html";;
let content_type_xml = "text/xml";;

type post_encoded = [`unknown | `url_encoded | `multipart_form_data];;

let post () = Lazy.force post;;

let decode_content_type (s: string) = (
	let content_type_value = String.lowercase_ascii s in
	if prefixed content_type_url_encoded content_type_value then (
		`url_encoded
	) else if prefixed content_type_multipart_form_data content_type_value then (
		`multipart_form_data
	) else (
		`unknown
	)
);;

let post_encoded () = (
	let content_type_value = getenv env_content_type in
	decode_content_type content_type_value
);;

let post_length () = (
	let content_length_value = getenv env_content_length in
	try int_of_string content_length_value with Failure _ -> 0
);;

let decode_cookie: string -> string StringMap.t = decode_query_string_or_cookie ';';;

let decode_query_string: string -> string StringMap.t = decode_query_string_or_cookie '&';;

let decode_multipart_form_data (source: string) = (
	let source_length = String.length source in
	let newline (i: int ref) = (
		if source.[!i] = '\r' then (
			incr i;
			if !i < source_length && source.[!i] = '\n' then incr i;
			true
		) else if source.[!i] = '\n' then (
			incr i;
			true
		) else (
			false
		)
	) in
	let get_string (i: int ref) = (
		if source.[!i] = '\"' then (
			incr i;
			let first = !i in
			while !i < source_length && source.[!i] <> '\"' do
				incr i
			done;
			let last = !i in
			incr i;
			String.sub source first (last - first)
		) else (
			""
		)
	) in
	let skip_spaces (i: int ref) = (
		while !i < source_length && source.[!i] = ' ' do
			incr i
		done
	) in
	let match_and_succ (sub: string) (i: int ref) = (
		let sub_length = String.length sub in
		if !i + sub_length <= source_length
			&& String.lowercase_ascii (String.sub source !i sub_length) = sub
		then (
			i := !i + sub_length;
			true
		) else (
			false
		)
	) in
	let remove_last_crlf (i: int) = (
		let i = if source.[i - 1] = '\n' then pred i else i in
		let i = if source.[i - 1] = '\r' then pred i else i in i
	) in
	let result = ref StringMap.empty in
	if source.[0] = '-' then (
		let i = ref 0 in
		while !i < source_length && not (newline i) do
			incr i;
		done;
		let boundary = String.sub source 0 (remove_last_crlf !i) in
		while !i < source_length do
			let (_: bool) = newline i in
			let next = try string_index_from source !i boundary with Not_found -> source_length in
			let last = remove_last_crlf next in
			if match_and_succ "content-disposition:" i then (
				skip_spaces i;
				if match_and_succ "form-data;" i then (
					skip_spaces i;
					if match_and_succ "name=" i then (
						let name = get_string i in
						if newline i then (
							let (_: bool) = newline i in
							result := StringMap.add name (String.sub source !i (last - !i)) !result
						) else if source.[!i] = ';' then (
							incr i;
							skip_spaces i;
							if match_and_succ "filename=" i then (
								let filename = get_string i in
								if newline i then (
									if match_and_succ "content-type:" i then (
										skip_spaces i;
										let ct_first = !i in
										while not (newline i) do
											incr i
										done;
										let ct_last = remove_last_crlf !i in
										let content_type = String.sub source ct_first (ct_last - ct_first) in
										let (_: bool) = newline i in
										result := StringMap.add name (String.sub source !i (last - !i)) !result;
										result := StringMap.add (name ^ ":filename") filename !result;
										result := StringMap.add (name ^ ":content-type") content_type !result;
									)
								)
							)
						)
					)
				)
			);
			i := next + (String.length boundary)
		done
	);
	!result
);;

let read_input () = (
	if post () then (
		let length = post_length () in
		set_binary_mode_in stdin true;
		let data = really_input_string stdin length in
		begin match post_encoded () with
		| `url_encoded -> decode_query_string data
		| `multipart_form_data -> decode_multipart_form_data data
		| `unknown -> StringMap.empty
		end
	) else (
		StringMap.empty
	)
);;

let encode_uri (print_string: string -> unit) (s: string) = (
	let buf1 = Bytes.make 1 ' ' in
	let buf3 = Bytes.make 3 '%' in
	for i = 0 to String.length s - 1 do
		begin match s.[i] with
		| ' ' ->
			print_string "+"
		| ('0'..'9' as c) | ('A'..'Z' as c) | ('a'..'z' as c) | (':' as c) | ('/' as c) | ('.' as c) ->
			Bytes.set buf1 0 c;
			print_string (Bytes.unsafe_to_string buf1)
		| _ as c ->
			let n = int_of_char c in
			Bytes.set buf3 1 (hex_of_int (n / 16));
			Bytes.set buf3 2 (hex_of_int (n mod 16));
			print_string (Bytes.unsafe_to_string buf3)
		end
	done
);;

let header_see_other (print_string: string -> unit) (uri: string) = (
	print_string "status: 303 See Other\n";
	print_string "location: ";
	print_string uri;
	print_string "\n"
);;

let header_service_unavailable (print_string: string -> unit) = (
	print_string "status: 503 Service Unavailable\n"
);;

let header_content_type (print_string: string -> unit) (content_type: string) = (
	print_string "content-type: ";
	print_string content_type;
	print_string "\n"
);;

let header_cookie (print_string: string -> unit) ?(expires: float option) (cookie: string StringMap.t) = (
	let expires_image = lazy (
		begin match expires with
		| Some time ->
			let t = Unix.gmtime time in
			Printf.sprintf " expires=%s, %.2d-%s-%.4d %.2d:%.2d:%.2d GMT;" (string_of_weekday t.Unix.tm_wday)
				t.Unix.tm_mday (string_of_month t.Unix.tm_mon) (t.Unix.tm_year + 1900)
				t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec
		| None -> ""
		end
	) in
	StringMap.iter (fun key value ->
		print_string "set-cookie: ";
		print_string key;
		print_string "=";
		encode_uri print_string value;
		print_string ";";
		print_string (Lazy.force expires_image);
		print_string "\n"
	) cookie
);;

let header_break (print_string: string -> unit) = (
	print_string "\n"
);;

module HTML = Web__HTML;;
