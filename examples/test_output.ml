let b = Buffer.create 0;;

(* HTTP header *)

let m = Web.StringMap.add "K" "V" Web.StringMap.empty in
Web.header_cookie (Buffer.add_substring b) ~expires:0. m;
assert (
	Buffer.contents b = "set-cookie: K=V; expires=Thu, 01 Jan 1970 00:00:00 GMT;\n"
);;

(* HTML text *)

Buffer.clear b;;

let count = ref 0 in
let tc =
	Web.HTML.open_text `html4 (fun s pos len ->
		incr count;
		Buffer.add_substring b s pos len
	)
in
count := 0;
Web.HTML.text_output_string tc "ABCDEFG";
assert (!count = 1 && Buffer.contents b = "ABCDEFG");
count := 0;
Web.HTML.text_output_substring tc ".&H<>I." 1 5;
assert (!count = 5 && Buffer.contents b = "ABCDEFG&amp;H&lt;&gt;I");
Web.HTML.close_text tc;;

Buffer.clear b;;

let out version = (
	let tc = Web.HTML.open_text version (Buffer.add_substring b) in
	Web.HTML.text_output_string tc "\n";
	Web.HTML.close_text tc
) in
out `html4;
out `xhtml1;
assert (Buffer.contents b = "<br><br />");;

Buffer.clear b;;

let tc = Web.HTML.open_text `html4 (Buffer.add_substring b) in
Web.HTML.text_output_string tc "\rA\r\nB\r\rC\r";
Web.HTML.close_text tc;
assert (Buffer.contents b = "<br>A<br>B<br><br>C<br>");;

Buffer.clear b;;

let tc = Web.HTML.open_text `html5 (Buffer.add_substring b) in
Web.HTML.text_output_string tc "\r";
Web.HTML.text_output_string tc "\n";
Web.HTML.close_text tc;
assert (Buffer.contents b = "<br>");;

(* HTML attribute *)

Buffer.clear b;;

let count = ref 0 in
let name = "NAME" in
let ac =
	Web.HTML.open_attribute `html4 (fun s pos len ->
		incr count;
		Buffer.add_substring b s pos len
	) name
in
count := 0;
Web.HTML.attribute_output_string ac "ABCDEFG";
assert (!count = 1 && Buffer.contents b = " NAME=\"ABCDEFG");
count := 0;
Web.HTML.attribute_output_substring ac ".&H<>I." 1 5;
assert (!count = 5 && Buffer.contents b = " NAME=\"ABCDEFG&amp;H&lt;&gt;I");
Web.HTML.close_attribute ac;;

Buffer.clear b;;

let out version = (
	Buffer.add_string b "<a";
	let ac = Web.HTML.open_attribute version (Buffer.add_substring b) "href" in
	Web.HTML.attribute_output_string ac "'";
	Web.HTML.close_attribute ac;
	Buffer.add_string b " />"
) in
out `xhtml1;
out `xhtml5;
assert (Buffer.contents b = "<a href=\"&#39;\" /><a href=\"&apos;\" />");;

Buffer.clear b;;

let out version name = (
	let ac = Web.HTML.open_attribute version (Buffer.add_substring b) name in
	Web.HTML.attribute_output_string ac "\rA\r\nB\r\rC\r";
	Web.HTML.close_attribute ac
) in
out `html4 "HTML4";
out `html5 "HTML5";
assert (
	Buffer.contents b
	= " HTML4=\"\nA\nB\n\nC\n\" HTML5=\"&#13;A&#13;&NewLine;B&#13;&#13;C&#13;\""
);;

Buffer.clear b;;

let out version name = (
	let ac = Web.HTML.open_attribute version (Buffer.add_substring b) name in
	Web.HTML.attribute_output_string ac "\r";
	Web.HTML.attribute_output_string ac "\n";
	Web.HTML.close_attribute ac
) in
out `html4 "HTML4";
out `html5 "HTML5";
assert (Buffer.contents b = " HTML4=\"\n\" HTML5=\"&#13;&NewLine;\"");;

prerr_endline "ok";;
