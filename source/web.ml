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

end;;
open Private;;

let application_x_www_form_urlencoded = "application/x-www-form-urlencoded";;
let multipart_form_data = "multipart/form-data";;
let text_html = "text/html";;
let text_plain = "text/plain";;
let text_xml = "text/xml";;

let array_find_index: string array -> string -> int =
	let rec loop i a s = (
		if a.(i) = s then i else
		if i < Array.length a then loop (i + 1) a s
		else raise (Failure "array_find_index")
	) in
	loop 0;;

let weekday_data = [|
	"Sun";
	"Mon";
	"Tue";
	"Wed";
	"Thu";
	"Fri";
	"Sat"
|];;

let string_of_weekday = Array.get weekday_data;;

let weekday_of_string = array_find_index weekday_data;;

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

let month_of_string = array_find_index month_data;;

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
	let loc = "Web.decode_date" (* __FUNCTION__ *) in
	if String.length s = 29 && s.[3] = ',' && s.[4] = ' ' && s.[7] = ' '
		&& s.[11] = ' ' && s.[16] = ' ' && s.[19] = ':' && s.[22] = ':' && s.[25] = ' '
		&& s.[26] = 'G' && s.[27] = 'M' && s.[28] = 'T'
	then
		Scanf.sscanf s "%3s, %2d %3s %4d %2d:%2d:%2d GMT%!"
			(fun tm_wday tm_mday mon year tm_hour tm_min tm_sec ->
				let tm =
					{Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon = month_of_string mon;
						tm_year = year - 1900; tm_wday = 0; tm_yday = 0; tm_isdst = false
					}
				in
				let result, normalized_tm = timegm tm in
				if normalized_tm.Unix.tm_wday = weekday_of_string tm_wday then result
				else invalid_arg loc
			)
	else invalid_arg loc
);;

let escape_uri (d: bytes) (d_pos: int) (c: char) = (
	Bytes.set d d_pos '%';
	let n = int_of_char c in
	Bytes.set d (d_pos + 1) (hex_of_int (n / 16));
	Bytes.set d (d_pos + 2) (hex_of_int (n mod 16));
	d_pos + 3
);;

let encode_uri: (bytes -> int -> char -> int) -> string -> string =
	let rec loop f s s_pos d d_pos = (
		if s_pos >= String.length s then (
			if d_pos < Bytes.length d then Bytes.sub_string d 0 d_pos
			else Bytes.unsafe_to_string d
		) else loop f s (s_pos + 1) d (f d d_pos s.[s_pos])
	) in
	fun f s -> loop f s 0 (Bytes.create (String.length s * 3)) 0;;

let unescape_uri (d: bytes) (d_pos: int) (s: string) (s_pos: int) = (
	assert (s.[s_pos] = '%');
	let s_length = String.length s in
	let s_pos = s_pos + 1 in (* skip '%' *)
	if s_pos < s_length && is_hex s.[s_pos] then (
		let hi = int_of_hex s.[s_pos] in
		let s_pos = s_pos + 1 in
		if s_pos < s_length && is_hex s.[s_pos] then (
			let lo = int_of_hex s.[s_pos] in
			Bytes.set d d_pos (char_of_int (hi * 16 + lo));
			s_pos + 1
		) else (
			Bytes.set d d_pos (char_of_int hi);
			s_pos
		)
	) else (
		Bytes.set d d_pos '\x00';
		s_pos
	)
);;

let decode_uri: (bytes -> int -> string -> int -> int) -> string -> string =
	let rec loop f s s_pos d d_pos = (
		if s_pos >= String.length s then (
			if d_pos < Bytes.length d then Bytes.sub_string d 0 d_pos
			else Bytes.unsafe_to_string d
		) else loop f s (f d d_pos s s_pos) d (d_pos + 1)
	) in
	fun f s -> loop f s 0 (Bytes.create (String.length s)) 0;;

let encode_uri_path: string -> string =
	encode_uri (fun d d_pos c ->
		match c with
		| '0'..'9' | 'A'..'Z' | 'a'..'z'
		| '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' (* unreserved *)
		| ':' | '@' | '&' | '=' | '+' | '$' | ',' (* unreserved for path *) as c ->
			Bytes.set d d_pos c;
			d_pos + 1
		| _ as c ->
			escape_uri d d_pos c
	);;

let decode_uri_path: string -> string =
	decode_uri (fun d d_pos s s_pos ->
		match s.[s_pos] with
		| '%' ->
			unescape_uri d d_pos s s_pos
		| _ as c ->
			Bytes.set d d_pos c;
			s_pos + 1
	);;

let encode_uri_query: string -> string =
	encode_uri (fun d d_pos c ->
		match c with
		| ' ' -> (* additional conversion for query *)
			Bytes.set d d_pos '+';
			d_pos + 1
		| '0'..'9' | 'A'..'Z' | 'a'..'z'
		| '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' (* unreserved *) as c ->
			Bytes.set d d_pos c;
			d_pos + 1
		| _ as c ->
			escape_uri d d_pos c
	);;

let decode_uri_query: string -> string =
	decode_uri (fun d d_pos s s_pos ->
		match s.[s_pos] with
		| '+' -> (* additional conversion for query *)
			Bytes.set d d_pos ' ';
			s_pos + 1
		| '%' ->
			unescape_uri d d_pos s s_pos
		| _ as c ->
			Bytes.set d d_pos c;
			s_pos + 1
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

type post_encoded = [`unknown | `urlencoded | `multipart_form_data];;

let decode_content_type (s: string) = (
	let content_type_value = String.lowercase_ascii s in
	if prefixed application_x_www_form_urlencoded content_type_value then (
		`urlencoded
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
	if not (StringMap.is_empty cookie) then (
		let expires_image =
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
		in
		StringMap.iter (fun key value ->
			print_string "set-cookie: ";
			print_string key;
			print_string "=";
			print_string (encode_uri_query value);
			print_string ";";
			print_string expires_image;
			print_string "\n"
		) cookie
	)
);;

let header_break (print_string: string -> unit) () = (
	print_string "\n"
);;

module CGI = Web__CGI
module HTML = Web__HTML;;
