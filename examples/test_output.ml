let b = Buffer.create 0;;

(* HTTP header *)

let m = Web.StringMap.add "K" "V" Web.StringMap.empty in
Web.header_cookie (Buffer.add_string b) ~expires:0. m;
assert (
	Buffer.contents b = "set-cookie: K=V; expires=Thu, 01 Jan 1970 00:00:00 GMT;\n"
);;

(* HTML text *)

Buffer.clear b;;

let out version = (
	let tc = Web.HTML.open_text version (Buffer.add_string b) in
	Web.HTML.text_output_string tc "\n";
	Web.HTML.close_text tc
) in
out `html4;
out `xhtml1;
assert (Buffer.contents b = "<br><br />");;

prerr_endline "ok";;
