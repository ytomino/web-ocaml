open Web;;

let getenv name = (
	try Sys.getenv name with Not_found -> ""
);;

let env_request_scheme = "REQUEST_SCHEME";;
let env_https = "HTTPS";;
let env_http_host = "HTTP_HOST";;
let env_server_name = "SERVER_NAME";;
let env_request_uri = "REQUEST_URI";;
let env_query_string = "QUERY_STRING";;
let env_http_cookie = "HTTP_COOKIE";;
let env_request_method = "REQUEST_METHOD";;
let env_content_type = "CONTENT_TYPE";;
let env_content_length = "CONTENT_LENGTH";;
let env_remote_addr = "REMOTE_ADDR";;
let env_remote_host = "REMOTE_HOST";;
let env_user_agent = "HTTP_USER_AGENT";;

let request_scheme () = (
	let request_scheme = getenv env_request_scheme in
	if String.length request_scheme <> 0 then request_scheme else
	let https = getenv env_https in
	if String.length https >= 2
		&& Char.lowercase_ascii https.[0] = 'o'
		&& Char.lowercase_ascii https.[1] = 'n'
	then "https"
	else "http"
);;

let host () = (
	let http_host = getenv env_http_host in
	if String.length http_host <> 0 then http_host
	else getenv env_server_name
);;

let request_uri () = (
	let request_uri = getenv env_request_uri in
	if not (String.contains request_uri '?') then (
		let query_string = getenv env_query_string in
		let query_string_length = String.length query_string in
		if query_string_length = 0 then request_uri else
		let request_uri_length = String.length request_uri in
		let result = Bytes.create (request_uri_length + 1 + query_string_length) in
		Bytes.blit_string request_uri 0 result 0 request_uri_length;
		Bytes.set result request_uri_length '?';
		Bytes.blit_string query_string 0 result (request_uri_length + 1)
			query_string_length;
		Bytes.unsafe_to_string result
	) else request_uri
);;

let request_path () = (
	let request_uri = getenv env_request_uri in
	match String.index_opt request_uri '?' with
	| None -> request_uri
	| Some pos -> String.sub request_uri 0 pos
);;

let query () = (
	let query_string =
		let request_uri = getenv env_request_uri in
		match String.index_opt request_uri '?' with
		| None ->
			getenv env_query_string
		| Some pos ->
			let pos = pos + 1 in (* skip '?' *)
			String.sub request_uri pos (String.length request_uri - pos)
	in
	decode_query_string query_string
);;

let cookie () = (
	let cookie = getenv env_http_cookie in
	decode_cookie cookie
);;

let post () = (
	let request_method_value = getenv env_request_method in
	String.lowercase_ascii request_method_value = "post"
);;

let post_encoded () = (
	let content_type_value = getenv env_content_type in
	decode_content_type content_type_value
);;

let post_length () = (
	let content_length_value = getenv env_content_length in
	try int_of_string content_length_value with Failure _ -> 0
);;

let read_post () = (
	if post () then (
		let length = post_length () in
		set_binary_mode_in stdin true;
		let data = really_input_string stdin length in
		begin match post_encoded () with
		| `urlencoded -> decode_query_string data
		| `multipart_form_data -> decode_multipart_form_data data
		| `unknown -> StringMap.empty
		end
	) else (
		StringMap.empty
	)
);;

let remote_addr () = getenv env_remote_addr;;

let remote_host () = getenv env_remote_host;;

let user_agent () = getenv env_user_agent;;
