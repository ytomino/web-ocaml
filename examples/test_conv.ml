(* URI *)

let check_uri encode_uri decode_uri decoded encoded = (
	String.equal (encode_uri decoded) encoded
	&& String.equal (decode_uri encoded) decoded
);;

assert (check_uri Web.encode_uri_query Web.decode_uri_query "" "");;
assert (check_uri Web.encode_uri_query Web.decode_uri_query "?" "%3f");;
assert (check_uri Web.encode_uri_query Web.decode_uri_query "??" "%3f%3f");;
assert (check_uri Web.encode_uri_query Web.decode_uri_query "0 1" "0+1");;

(* Content-type *)

assert (Web.decode_content_type "" = `unknown);;
assert (
	Web.decode_content_type "application/x-www-form-urlencoded" = `url_encoded
);;
assert (Web.decode_content_type "multipart/form-data" = `multipart_form_data);;
assert (
	Web.decode_content_type "multipart/form-data; boundary=BOUNDARY"
	= `multipart_form_data
);;

prerr_endline "ok";;
