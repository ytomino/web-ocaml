(* URI *)

assert (Web.encode_uri_query "" = "");;
assert (Web.encode_uri_query "?" = "%3f");;
assert (Web.encode_uri_query "??" = "%3f%3f");;
assert (Web.encode_uri_query "0 1" = "0+1");;

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
