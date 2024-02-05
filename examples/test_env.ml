(* REQUEST_URI and QUERY_STRING *)

let env_request_uri = "REQUEST_URI";;
let env_query_string = "QUERY_STRING";;

(* none *)
Unix.putenv env_request_uri "/path";;
Unix.putenv env_query_string "";;
assert (Web.CGI.request_uri () = "/path");;
assert (Web.CGI.request_path () = "/path");;
(* by REQUEST_URI *)
Unix.putenv env_request_uri "/path?NAME=VALUE";;
Unix.putenv env_query_string "";;
assert (Web.CGI.request_uri () = "/path?NAME=VALUE");;
assert (Web.CGI.request_path () = "/path");;
(* by QUERY_STRING *)
Unix.putenv env_request_uri "/path";;
Unix.putenv env_query_string "NAME=VALUE";;
assert (Web.CGI.request_uri () = "/path?NAME=VALUE");;
assert (Web.CGI.request_path () = "/path");;
(* both *)
Unix.putenv env_request_uri "/path?NAME=VALUE";;
Unix.putenv env_query_string "NAME=VALUE";;
assert (Web.CGI.request_uri () = "/path?NAME=VALUE");;
assert (Web.CGI.request_path () = "/path");;

prerr_endline "ok";;
