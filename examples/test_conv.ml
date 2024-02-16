(* Date *)

let posix_zero_time = "Thu, 01 Jan 1970 00:00:00 GMT" in
assert (Web.encode_date 0. = posix_zero_time);
assert (Web.decode_date posix_zero_time = 0.);;

let the_time = "Sun, 31 Dec 2000 23:59:59 GMT" in
assert (Web.encode_date (Web.decode_date the_time) = the_time);;

let the_time = "Mon, 31 Dec 2000 23:59:59 GMT" in (* bad weekday *)
assert (
	try let _: float = Web.decode_date the_time in false with
	| Invalid_argument _ -> true
);;

(* URI *)

let check_uri encode_uri decode_uri decoded encoded = (
	String.equal (encode_uri decoded) encoded
	&& String.equal (decode_uri encoded) decoded
);;

List.iter (fun (encode_uri, decode_uri) ->
	assert (check_uri encode_uri decode_uri "" "");
	assert (check_uri encode_uri decode_uri "-_.!~*'()" "-_.!~*'()");
	assert (check_uri encode_uri decode_uri "?" "%3f");
	assert (check_uri encode_uri decode_uri "??" "%3f%3f")
) [
	Web.encode_uri_path, Web.decode_uri_path;
	Web.encode_uri_query, Web.decode_uri_query
];;
assert (check_uri Web.encode_uri_path Web.decode_uri_path "0 1" "0%201");;
assert (check_uri Web.encode_uri_query Web.decode_uri_query "0 1" "0+1");;
assert (
	check_uri Web.encode_uri_path Web.decode_uri_path ":@&=+$," ":@&=+$,"
);;
assert (
	check_uri Web.encode_uri_query Web.decode_uri_query
		":@&=+$," "%3a%40%26%3d%2b%24%2c"
);;

(* Query *)

let m = Web.decode_query_string "" in
assert (Web.StringMap.is_empty m);;

let m = Web.decode_query_string "namae=%E5%B1%B1%E7%94%B0" in
assert (Web.StringMap.find "namae" m = "山田");;

let m = Web.decode_query_string "name=Yamada&namae=%E5%B1%B1%E7%94%B0" in
assert (Web.StringMap.find "name" m = "Yamada");
assert (Web.StringMap.find "namae" m = "山田");;

let m = Web.decode_query_string "name=&namae=" in
assert (Web.StringMap.find "name" m = "");
assert (Web.StringMap.find "namae" m = "");;

let m = Web.decode_query_string "nameonly&namaeonly" in
assert (Web.StringMap.find "nameonly" m = "");
assert (Web.StringMap.find "namaeonly" m = "");;

(* Cookie *)

let m = Web.decode_cookie "name=Yamada; namae=%E5%B1%B1%E7%94%B0" in
assert (Web.StringMap.find "name" m = "Yamada");
assert (Web.StringMap.find "namae" m = "山田");;

(* Content-type *)

assert (Web.decode_content_type "" = `unknown);;
assert (
	Web.decode_content_type "application/x-www-form-urlencoded" = `urlencoded
);;
assert (Web.decode_content_type "multipart/form-data" = `multipart_form_data);;
assert (
	Web.decode_content_type "multipart/form-data; boundary=BOUNDARY"
	= `multipart_form_data
);;

(* multipart/form-data *)

let m =
	Web.decode_multipart_form_data "\
		---boundary\n\
		Content-Disposition: form-data; name=\"name\"\n\
		\n\
		Yamada\n\
		---boundary\n\
		Content-Disposition: form-data; name=\"namae\"\n\
		\n\
		山田\n\
		---boundary--\n"
in
assert (Web.StringMap.find "name" m = "Yamada");
assert (Web.StringMap.find "namae" m = "山田");;

let m =
	Web.decode_multipart_form_data "\
		---boundary\n\
		Content-Disposition: form-data; name=\"name\"; filename=\"in.txt\"\n\
		Content-Type: text/plain\n\
		\n\
		value\n\
		---boundary--\n"
in
assert (Web.StringMap.find "name" m = "value");
assert (Web.StringMap.find "name:filename" m = "in.txt");
assert (Web.StringMap.find "name:content-type" m = Web.text_plain);;

prerr_endline "ok";;
