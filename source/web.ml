module StringMap = Map.Make(String);;

let application_x_www_form_urlencoded = "application/x-www-form-urlencoded";;
let multipart_form_data = "multipart/form-data";;
let text_html = "text/html";;
let text_plain = "text/plain";;
let text_xml = "text/xml";;

let array_get (a: string array) (i: int) loc = (
	if i >= 0 && i < Array.length a then a.(i)
	else invalid_arg loc
);;

let array_find_index: string array -> string -> string -> int =
	let rec loop i a s loc = (
		if a.(i) = s then i else
		if i < Array.length a then loop (i + 1) a s loc
		else invalid_arg loc
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

let string_of_weekday = array_get weekday_data;;

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

let string_of_month = array_get month_data;;

let month_of_string = array_find_index month_data;;

let encode_date (time: float) = (
	let loc = "Web.encode_date" (* __FUNCTION__ *) in
	let t = Unix.gmtime time in
	Printf.sprintf "%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT"
		(string_of_weekday t.Unix.tm_wday loc) t.Unix.tm_mday
		(string_of_month t.Unix.tm_mon loc) (t.Unix.tm_year + 1900) t.Unix.tm_hour
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
					{Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon = month_of_string mon loc;
						tm_year = year - 1900; tm_wday = 0; tm_yday = 0; tm_isdst = false
					}
				in
				let result, normalized_tm = timegm tm in
				if normalized_tm.Unix.tm_wday = weekday_of_string tm_wday loc then result
				else invalid_arg loc
			)
	else invalid_arg loc
);;

let hex_of_int n = (
	"0123456789abcdef".[n]
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

let is_hex c = (
	begin match c with
	| '0'..'9' | 'A'..'F' | 'a'..'f' -> true
	| _ -> false
	end
);;

let int_of_hex c loc = (
	begin match c with
	| '0'..'9' -> int_of_char c - int_of_char '0'
	| 'A'..'F' -> int_of_char c - (int_of_char 'A' - 10)
	| 'a'..'f' -> int_of_char c - (int_of_char 'a' - 10)
	| _ -> invalid_arg loc
	end
);;

let unescape_uri (d: bytes) (d_pos: int) (s: string) (s_pos: int) loc = (
	assert (s.[s_pos] = '%');
	let s_length = String.length s in
	let s_pos = s_pos + 1 in (* skip '%' *)
	if s_pos < s_length && is_hex s.[s_pos] then (
		let hi = int_of_hex s.[s_pos] loc in
		let s_pos = s_pos + 1 in
		if s_pos < s_length && is_hex s.[s_pos] then (
			let lo = int_of_hex s.[s_pos] loc in
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
		let loc = "Web.decode_uri_path" (* __FUNCTION__ *) in
		match s.[s_pos] with
		| '%' ->
			unescape_uri d d_pos s s_pos loc
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
		let loc = "Web.decode_uri_query" (* __FUNCTION__ *) in
		match s.[s_pos] with
		| '+' -> (* additional conversion for query *)
			Bytes.set d d_pos ' ';
			s_pos + 1
		| '%' ->
			unescape_uri d d_pos s s_pos loc
		| _ as c ->
			Bytes.set d d_pos c;
			s_pos + 1
	);;

let encode_query_string_like (separator: char) (m: string StringMap.t) = (
	let max_length =
		StringMap.fold (fun name value max_length ->
			(if max_length = 0 then 0 else max_length + 1)
			+ String.length name + 1 + String.length value * 3
		) m 0
	in
	let result = Bytes.create max_length in
	let length =
		StringMap.fold (fun name value length ->
			let next =
				if length = 0 then 0
				else (
					Bytes.set result length separator;
					length + 1
				)
			in
			let name_length = String.length name in
			String.blit name 0 result next name_length;
			let next = next + name_length in
			Bytes.set result next '=';
			let next = next + 1 in
			let encoded_value = encode_uri_query value in
			let encoded_value_length = String.length encoded_value in
			String.blit encoded_value 0 result next encoded_value_length;
			next + encoded_value_length
		) m 0
	in
	if length < max_length then Bytes.sub_string result 0 length
	else Bytes.unsafe_to_string result
);;

let rec skip_spaces (s: string) (i: int) = (
	if i >= String.length s || s.[i] <> ' ' then i
	else skip_spaces s (i + 1)
);;

let decode_query_string_or_cookie: char -> (string -> int -> int) -> string ->
	string StringMap.t =
	let rec loop i result separator succ s = (
		let s_length = String.length s in
		if i >= s_length then result else
		let next =
			match String.index_from_opt s i separator with
			| None -> s_length
			| Some next -> next
		in
		let eq_pos, value =
			match String.index_from_opt s i '=' with
			| None ->
				next, ""
			| Some eq_pos ->
				let value_pos = eq_pos + 1 in
				eq_pos, decode_uri_query (String.sub s value_pos (next - value_pos))
		in
		let name = String.sub s i (eq_pos - i) in
		loop (succ s next) (StringMap.add name value result) separator succ s
	) in
	loop 0 StringMap.empty;;

let encode_query_string: string StringMap.t -> string =
	encode_query_string_like '&';;

let decode_query_string: string -> string StringMap.t =
	decode_query_string_or_cookie '&' (fun _ i -> i + 1);;

let decode_cookie: string -> string StringMap.t =
	decode_query_string_or_cookie ';' (fun s i -> skip_spaces s (i + 1));;

type post_encoded = [`unknown | `urlencoded | `multipart_form_data];;

let insensitive_starts_with_from (sub: string) (s: string) (pos: int) = (
	assert (pos >= 0);
	let sub_length = String.length sub in
	assert (sub_length > 0);
	let s_length = String.length s in
	if pos + sub_length > s_length || Char.lowercase_ascii s.[pos] <> sub.[0]
	then (
		false
	) else if pos > 0 || sub_length < s_length then (
		String.lowercase_ascii (String.sub s pos sub_length) = sub
	) else (
		String.lowercase_ascii s = sub
	)
);;

let decode_content_type (s: string) = (
	let prefixed sub s = (
		insensitive_starts_with_from sub s 0
		&& (
			let sub_length = String.length sub in
			String.length s = sub_length || s.[sub_length] = ';'
		)
	) in
	if prefixed application_x_www_form_urlencoded s then (
		`urlencoded
	) else if prefixed multipart_form_data s then (
		`multipart_form_data
	) else (
		`unknown
	)
);;

let decode_multipart_form_data: string -> string StringMap.t =
	let rec index_boundary_from s i boundary = (
		let submatch s i boundary boundary_pos = (
			let boundary_length = String.length boundary in
			if String.sub s boundary_pos boundary_length = boundary
			then i, boundary_pos + boundary_length
			else index_boundary_from s (i + 1) boundary
		) in
		let s_length = String.length s in
		let boundary_length = String.length boundary in
		if i + 1 + boundary_length > s_length then s_length, s_length else
		match s.[i] with
		| '\n' ->
			submatch s i boundary (i + 1)
		| '\r' ->
			if i + 2 + boundary_length <= s_length && s.[i + 1] = '\n'
			then submatch s i boundary (i + 2)
			else submatch s i boundary (i + 1)
		| _ ->
			index_boundary_from s (i + 1) boundary
	) in
	let newline s i = (
		let s_length = String.length s in
		if i >= s_length then None else
		match s.[i] with
		| '\n' ->
			Some (i + 1)
		| '\r' ->
			Some (
				let next = i + 1 in
				if next < s_length && s.[next] = '\n' then next + 1
				else next
			)
		| _ ->
			None
	) in
	let get_string s i = (
		if s.[i] = '"' then (
			let value_pos = i + 1 in
			let value_end, quot_end =
				match String.index_from_opt s value_pos '\"' with
				| None ->
					let s_length = String.length s in
					s_length, s_length
				| Some quot_pos ->
					quot_pos, quot_pos + 1
			in
			let value = String.sub s value_pos (value_end - value_pos) in
			quot_end, value
		) else i, ""
	) in
	let match_and_succ sub s i = (
		if insensitive_starts_with_from sub s i then Some (i + String.length sub)
		else None
	) in
	let rec on_boundary_newline s i boundary result = (
		match newline s i with
		| None ->
			skip s i boundary result
		| Some next ->
			on_header s next boundary result
	) and skip s i boundary result = (
		let s_length = String.length s in
		assert (i >= 0 && i <= s_length);
		if i >= s_length then result else
		let _, boundary_end = index_boundary_from s i boundary in
		on_boundary_newline s boundary_end boundary result
	) and on_text s i boundary result name = (
		assert (i >= 0 && i <= String.length s);
		match newline s i with
		| None ->
			skip s i boundary result
		| Some value_pos ->
			let value_end, boundary_end = index_boundary_from s value_pos boundary in
			let value = String.sub s value_pos (value_end - value_pos) in
			let result = StringMap.add name value result in
			on_boundary_newline s boundary_end boundary result
	) and on_file: string -> int -> string -> string StringMap.t -> string ->
		string -> string StringMap.t =
		let rec index_newline s i = (
			if i >= String.length s then i, i else
			match newline s i with
			| None ->
				index_newline s (i + 1)
			| Some next ->
				i, next
		) in
		fun s i boundary result name filename ->
		match match_and_succ "content-type:" s i with
		| None ->
			skip s i boundary result
		| Some i ->
			let ct_pos = skip_spaces s i in
			let ct_end, i = index_newline s ct_pos in
			begin match newline s i with
			| None ->
				skip s i boundary result
			| Some value_pos ->
				let value_end, boundary_end = index_boundary_from s value_pos boundary in
				let value = String.sub s value_pos (value_end - value_pos) in
				let content_type = String.sub s ct_pos (ct_end - ct_pos) in
				let result = StringMap.add name value result in
				let result = StringMap.add (name ^ ":filename") filename result in
				let result = StringMap.add (name ^ ":content-type") content_type result in
				on_boundary_newline s boundary_end boundary result
			end
	and on_header s i boundary result = (
		assert (i >= 0 && i <= String.length s);
		match match_and_succ "content-disposition:" s i with
		| None ->
			skip s i boundary result
		| Some i ->
			let i = skip_spaces s i in
			begin match match_and_succ "form-data;" s i with
			| None ->
				skip s i boundary result
			| Some i ->
				let i = skip_spaces s i in
				begin match match_and_succ "name=" s i with
				| None ->
					skip s i boundary result
				| Some i ->
					let i, name = get_string s i in
					begin match newline s i with
					| None ->
						if s.[i] = ';' then (
							let i = skip_spaces s (i + 1) in
							match match_and_succ "filename=" s i with
							| None ->
								skip s i boundary result
							| Some i ->
								let i, filename = get_string s i in
								begin match newline s i with
								| None ->
									skip s i boundary result
								| Some next ->
									on_file s next boundary result name filename
								end
						) else skip s i boundary result
					| Some next ->
						on_text s next boundary result name
					end
				end
			end
	) and on_first_boundary s i = (
		assert (i >= 0 && i <= String.length s);
		if i >= String.length s then StringMap.empty else
		match newline s i with
		| None ->
			on_first_boundary s (i + 1)
		| Some next ->
			on_header s next (String.sub s 0 i) StringMap.empty
	) in
	fun s ->
	if s.[0] = '-' then on_first_boundary s 1
	else StringMap.empty;;

let make_print_string (print_substring: string -> int -> int -> unit)
	(s: string) =
(
	print_substring s 0 (String.length s)
);;

let header_see_other (print_substring: string -> int -> int -> unit)
	(uri: string) =
(
	let print_string = make_print_string print_substring in
	print_string "status: 303 See Other\n";
	print_string "location: ";
	print_string uri;
	print_string "\n"
);;

let header_service_unavailable (print_substring: string -> int -> int -> unit)
	() =
(
	let print_string = make_print_string print_substring in
	print_string "status: 503 Service Unavailable\n"
);;

let header_content_type (print_substring: string -> int -> int -> unit)
	(content_type: string) =
(
	let print_string = make_print_string print_substring in
	print_string "content-type: ";
	print_string content_type;
	print_string "\n"
);;

let header_cookie (print_substring: string -> int -> int -> unit)
	?(expires: float option) (cookie: string StringMap.t) =
(
	if not (StringMap.is_empty cookie) then (
		let print_string = make_print_string print_substring in
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

let header_break (print_substring: string -> int -> int -> unit) () = (
	let print_string = make_print_string print_substring in
	print_string "\n"
);;

module CGI = Web__CGI
module HTML = Web__HTML;;
