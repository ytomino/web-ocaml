open Web;;

let getenv name = (
	try Sys.getenv name with Not_found -> ""
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

let request_uri () = (
	let request_uri_value = getenv env_request_uri in
	let query_string_value = getenv env_query_string in
	if query_string_value = "" || String.contains request_uri_value '?' then (
		request_uri_value
	) else (
		request_uri_value ^ "?" ^ query_string_value
	)
);;

let post () = Lazy.force post;;

let post_encoded () = (
	let content_type_value = getenv env_content_type in
	decode_content_type content_type_value
);;

let post_length () = (
	let content_length_value = getenv env_content_length in
	try int_of_string content_length_value with Failure _ -> 0
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

let remote_addr () = getenv env_remote_addr;;

let remote_host () = getenv env_remote_host;;
