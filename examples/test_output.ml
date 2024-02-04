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

(* HTML attribute *)

Buffer.clear b;;

let out version = (
	Buffer.add_string b "<a";
	let ac = Web.HTML.open_attribute version (Buffer.add_string b) "href" in
	Web.HTML.attribute_output_string ac "'";
	Web.HTML.close_attribute ac;
	Buffer.add_string b " />"
) in
out `xhtml1;
out `xhtml5;
assert (Buffer.contents b = "<a href=\"&#39;\" /><a href=\"&apos;\" />");;

prerr_endline "ok";;
