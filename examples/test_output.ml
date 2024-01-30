let b = Buffer.create 0;;

(* HTTP header *)

let m = Web.StringMap.add "K" "V" Web.StringMap.empty in
Web.header_cookie (Buffer.add_string b) ~expires:0. m;
assert (
	Buffer.contents b = "set-cookie: K=V; expires=Thu, 01 Jan 1970 00:00:00 GMT;\n"
);;

prerr_endline "ok";;
