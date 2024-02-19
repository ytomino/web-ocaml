let b = Buffer.create 0;;

(* HTTP header *)

let m = Web.StringMap.add "K" "V" Web.StringMap.empty in
Web.header_cookie (Buffer.add_substring b) ~expires:0. m;
assert (
	Buffer.contents b = "set-cookie: K=V; expires=Thu, 01 Jan 1970 00:00:00 GMT;\n"
);;

(* Query *)

Buffer.clear b;;

let out append m = (
	let qc = Web.open_query_string ?append (Buffer.add_substring b) in
	Web.query_string_output_map qc m;
	Web.close_query_string qc
) in
out None Web.StringMap.empty;
out (Some `path) Web.StringMap.empty;
out (Some `query) Web.StringMap.empty;
assert (Buffer.contents b = "");
out None (let open Web.StringMap in add "L" "M" (add "K" "V" empty));
out (Some `query) (let open Web.StringMap in add "q" "?&=" empty);
assert (Buffer.contents b = "K=V&L=M&q=%3f%26%3d");;

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
	let tc = Web.HTML.open_text version ~newline:`br (Buffer.add_substring b) in
	Web.HTML.text_output_string tc "\n";
	Web.HTML.close_text tc
) in
out `html4;
out `xhtml1;
assert (Buffer.contents b = "<br><br />");;

Buffer.clear b;;

let out space s = (
	let tc = Web.HTML.open_text `html4 ?space (Buffer.add_substring b) in
	Web.HTML.text_output_string tc s;
	Web.HTML.close_text tc
) in
out None "A B  C <  > \n  \n";
out (Some `nbsp) "A B  C <  > \n  \n";
out (Some `nbsp_boundary) "A B  C <  > \n  \n";
out (Some `nbsp_boundary) " A ";
out (Some `nbsp_boundary) "  A  ";
out (Some `nbsp_boundary) " & ";
out (Some `nbsp_boundary) "  &  ";
out (Some `nbsp_boundary) " \n ";
out (Some `nbsp_boundary) "  \n  ";
assert (
	Buffer.contents b = "\
		A B  C &lt;  &gt; \n  \n\
		A&nbsp;B&nbsp;&nbsp;C&nbsp;&lt;&nbsp;&nbsp;&gt;&nbsp;\n&nbsp;&nbsp;\n\
		A B&nbsp;&nbsp;C &lt;&nbsp;&nbsp;&gt;&nbsp;\n&nbsp;&nbsp;\n\
		&nbsp;A&nbsp;\
		&nbsp;&nbsp;A&nbsp;&nbsp;\
		&nbsp;&amp;&nbsp;\
		&nbsp;&nbsp;&amp;&nbsp;&nbsp;\
		&nbsp;\n&nbsp;\
		&nbsp;&nbsp;\n&nbsp;&nbsp;"
);;

Buffer.clear b;;

let out newline = (
	let tc = Web.HTML.open_text `html4 ?newline (Buffer.add_substring b) in
	Web.HTML.text_output_string tc "\rA\r\nB\r\rC\r";
	Web.HTML.close_text tc
) in
out None;
out (Some `br);
assert (Buffer.contents b = "\nA\nB\n\nC\n<br>A<br>B<br><br>C<br>");;

Buffer.clear b;;

let out newline = (
	let tc = Web.HTML.open_text `html5 ?newline (Buffer.add_substring b) in
	Web.HTML.text_output_string tc "\r";
	Web.HTML.text_output_string tc "\n";
	Web.HTML.close_text tc
) in
out None;
out (Some `br);
assert (Buffer.contents b = "\n<br>");;

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

(* Query into HTML attribute *)

Buffer.clear b;;

let ac = Web.HTML.open_attribute `html5 (Buffer.add_substring b) "href" in
Web.HTML.attribute_output_string ac "http://example.net/";
let qc =
	Web.open_query_string ~append:`path (Web.HTML.attribute_output_substring ac)
in
Web.query_string_output_map qc (let open Web.StringMap in add "K" "V" empty);
Web.query_string_output_map qc (let open Web.StringMap in add "L" "W" empty);
Web.close_query_string qc;
Web.HTML.close_attribute ac;
assert (Buffer.contents b = " href=\"http://example.net/?K=V&amp;L=W\"");;

prerr_endline "ok";;
