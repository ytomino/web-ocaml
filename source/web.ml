module StringMap = Map.Make(String);;

module Private = struct

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

end;;
open Private;;

let application_x_www_form_urlencoded = "application/x-www-form-urlencoded";;
let multipart_form_data = "multipart/form-data";;
let text_html = "text/html";;
let text_plain = "text/plain";;
let text_xml = "text/xml";;

let month_data = [|
	"Jan";
	"Feb";
	"Mar";
	"Apr";
	"May";
	"Jun";
	"Jul";
	"Aug";
	"Sep";
	"Oct";
	"Nov";
	"Dec"
|];;

let string_of_month = Array.get month_data;;

let month_of_string: string -> int =
	let rec loop i s = (
		if month_data.(i) = s then i else
		if i < 11 then loop (i + 1) s
		else raise (Failure "month_of_string")
	) in
	loop 0;;

let encode_date (time: float) = (
	let t = Unix.gmtime time in
	Printf.sprintf "%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT"
		(string_of_weekday t.Unix.tm_wday) t.Unix.tm_mday
		(string_of_month t.Unix.tm_mon) (t.Unix.tm_year + 1900) t.Unix.tm_hour
		t.Unix.tm_min t.Unix.tm_sec
);;

let timegm (tm: Unix.tm) = (
	let env_tz = "TZ" in
	let original_tz = Sys.getenv_opt env_tz in
	Fun.protect ~finally:(fun () ->
		Unix.putenv env_tz (
			match original_tz with
			| None -> ""
			| Some value -> value
		)
	) (fun () ->
		Unix.putenv env_tz "UTC";
		Unix.mktime tm
	)
);;

let decode_date (s: string) = (
	if String.length s = 29 && s.[3] = ',' && s.[4] = ' ' && s.[7] = ' '
		&& s.[11] = ' ' && s.[16] = ' ' && s.[19] = ':' && s.[22] = ':' && s.[25] = ' '
		&& s.[26] = 'G' && s.[27] = 'M' && s.[28] = 'T'
	then
		Scanf.sscanf s "%3s, %2d %3s %4d %2d:%2d:%2d GMT%!"
			(fun _ tm_mday mon year tm_hour tm_min tm_sec ->
				let tm =
					{Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon = month_of_string mon;
						tm_year = year - 1900; tm_wday = 0; tm_yday = 0; tm_isdst = false
					}
				in
				fst (timegm tm)
			)
	else invalid_arg "Web.time_of_string"
);;

let encode_uri_query (s: string) = (
	let rec loop s s_pos d d_pos = (
		if s_pos >= String.length s then (
			if d_pos < Bytes.length d then Bytes.sub_string d 0 d_pos
			else Bytes.unsafe_to_string d
		) else
		begin match s.[s_pos] with
		| ' ' ->
			Bytes.set d d_pos '+';
			loop s (s_pos + 1) d (d_pos + 1)
		| ('0'..'9' as c) | ('A'..'Z' as c) | ('a'..'z' as c) | (':' as c) | ('/' as c) | ('.' as c) ->
			Bytes.set d d_pos c;
			loop s (s_pos + 1) d (d_pos + 1)
		| _ as c ->
			Bytes.set d d_pos '%';
			let n = int_of_char c in
			Bytes.set d (d_pos + 1) (hex_of_int (n / 16));
			Bytes.set d (d_pos + 2) (hex_of_int (n mod 16));
			loop s (s_pos + 1) d (d_pos + 3)
		end
	) in
	loop s 0 (Bytes.create (String.length s * 3)) 0
);;

let decode_uri_query (source: string) = (
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

let rec skip_spaces (s: string) (i: int) = (
	if i >= String.length s || s.[i] <> ' ' then i
	else skip_spaces s (i + 1)
);;

let decode_query_string_or_cookie: char -> (string -> int -> int) -> string ->
	string StringMap.t =
	let rec loop i result separator succ source = (
		let source_length = String.length source in
		if i < source_length then (
			let next = try String.index_from source i separator with Not_found -> source_length in
			let sub_length = next - i in
			let sub = String.sub source i (next - i) in
			let eq_pos = try String.index sub '=' with Not_found -> sub_length in
			let name = String.sub sub 0 eq_pos in
			let value =
				decode_uri_query (String.sub sub (eq_pos + 1) (sub_length - (eq_pos + 1)))
			in
			loop (succ source next) (StringMap.add name value result) separator succ source
		) else (
			result
		)
	) in
	loop 0 StringMap.empty;;

let decode_query_string: string -> string StringMap.t =
	decode_query_string_or_cookie '&' (fun _ i -> i + 1);;

let decode_cookie: string -> string StringMap.t =
	decode_query_string_or_cookie ';' (fun s i -> skip_spaces s (i + 1));;

type post_encoded = [`unknown | `url_encoded | `multipart_form_data];;

let decode_content_type (s: string) = (
	let content_type_value = String.lowercase_ascii s in
	if prefixed application_x_www_form_urlencoded content_type_value then (
		`url_encoded
	) else if prefixed multipart_form_data content_type_value then (
		`multipart_form_data
	) else (
		`unknown
	)
);;

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
				i := skip_spaces source !i;
				if match_and_succ "form-data;" i then (
					i := skip_spaces source !i;
					if match_and_succ "name=" i then (
						let name = get_string i in
						if newline i then (
							let (_: bool) = newline i in
							result := StringMap.add name (String.sub source !i (last - !i)) !result
						) else if source.[!i] = ';' then (
							incr i;
							i := skip_spaces source !i;
							if match_and_succ "filename=" i then (
								let filename = get_string i in
								if newline i then (
									if match_and_succ "content-type:" i then (
										i := skip_spaces source !i;
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

let header_see_other (print_string: string -> unit) (uri: string) = (
	print_string "status: 303 See Other\n";
	print_string "location: ";
	print_string uri;
	print_string "\n"
);;

let header_service_unavailable (print_string: string -> unit) () = (
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
			let date = encode_date time in
			let date_length = String.length date in
			let result = Bytes.create (9 + date_length + 1) in
			String.blit " expires=" 0 result 0 9;
			String.blit date 0 result 9 date_length;
			Bytes.set result (9 + date_length) ';';
			Bytes.unsafe_to_string result
		| None -> ""
		end
	) in
	StringMap.iter (fun key value ->
		print_string "set-cookie: ";
		print_string key;
		print_string "=";
		print_string (encode_uri_query value);
		print_string ";";
		print_string (Lazy.force expires_image);
		print_string "\n"
	) cookie
);;

let header_break (print_string: string -> unit) () = (
	print_string "\n"
);;

module CGI = Web__CGI
module HTML = Web__HTML;;
