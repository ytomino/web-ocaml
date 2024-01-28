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
