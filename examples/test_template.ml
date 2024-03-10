let b = Buffer.create 0;;

let t = Web.Template.parse_string "" in
assert (Web.Template.is_empty t && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = "");;

let s = "No special tag." in
let t = Web.Template.parse_string s in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = "No special tag.");;

Buffer.clear b;;

let t = Web.Template.parse_substring "Substring." 1 8 in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = "ubstring");;

Buffer.clear b;;

let s = "<?xml version=\"1.0\"?><body></body>" in (* ignore XML declaration *)
let t = Web.Template.parse_string s in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = s);;

(* tag placeholders *)

Buffer.clear b;;

let t = Web.Template.parse_string "Before<?TAG />After." in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "TAG" ->
			assert (Web.Template.is_empty subt && Web.Template.kind subt = `element);
			print_substring " & " 0 3;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "Before & After.");;

Buffer.clear b;;

let t = Web.Template.parse_string "<?TAG></?TAG>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "TAG" ->
			assert (Web.Template.is_empty subt && Web.Template.kind subt = `element);
			let s = "Alternative." in
			print_substring s 0 (String.length s);
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "Alternative.");;

Buffer.clear b;;

let t = Web.Template.parse_string "<?TAG>Contents.</?TAG>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "TAG" ->
			assert (not (Web.Template.is_empty subt) && Web.Template.kind subt = `element);
			Web.Template.output_template print_substring subt;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "Contents.");;

Buffer.clear b;;

let t = Web.Template.parse_string "Before<?S1>1</?S1> &<?S2>2</?S2> After." in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_subtemplate (Buffer.add_substring b) t "S1";
assert (Buffer.contents b = "1");
Web.Template.output_subtemplate (Buffer.add_substring b) t "S2";
assert (Buffer.contents b = "12");
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "S1" | "S2" ->
			assert (not (Web.Template.is_empty subt) && Web.Template.kind subt = `element);
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 2);
assert (Buffer.contents b = "12Before & After.");;

Buffer.clear b;;

let t = Web.Template.parse_string "{<?OUTER>[<?INNER/>]</?OUTER>}" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let rec handler tag acc print_substring subt = (
	match tag with
	| "OUTER" ->
		assert (not (Web.Template.is_empty subt) && Web.Template.kind subt = `element);
		(* It is recommended not to do tailcall for backtracing. *)
		let acc =
			Web.Template.output_template_with handler acc print_substring subt
		in
		acc + 1
	| "INNER" ->
		assert (Web.Template.is_empty subt && Web.Template.kind subt = `element);
		print_substring "()" 0 2;
		acc + 1
	| _ ->
		Web.Template.unhandled tag
) in
let acc =
	Web.Template.output_template_with handler 0 (Buffer.add_substring b) t
in
assert (acc = 2);
assert (Buffer.contents b = "{[()]}");;

Buffer.clear b;;

let s = "\
	<body>\n\
	\ <?OUTER>\n\
	\  <?INNER1>\n\
	\   <p>1</p>\n\
	\  </?INNER1>\n\
	\  <?INNER2>\n\
	\   <p>2</p>
	\  </?INNER2>\n\
	\ </?OUTER>\n\
	</body>\n"
in
let t = Web.Template.parse_string s in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let rec handler tag acc print_substring subt = (
	match tag with
	| "OUTER" | "INNER1" | "INNER2" ->
		assert (not (Web.Template.is_empty subt) && Web.Template.kind subt = `element);
		let acc = Web.Template.output_template_with handler acc print_substring subt in
		acc + 1
	| _ ->
		Web.Template.unhandled tag
) in
let acc =
	Web.Template.output_template_with handler 0 (Buffer.add_substring b) t
in
assert (acc = 3);
assert (
	Buffer.contents b
	= "\
		<body>\n\
		\   <p>1</p>\n\
		\   <p>2</p>\n\
		</body>\n"
);;

Buffer.clear b;;

let t = Web.Template.parse_string "<body><?>IGNORED</?></body>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = "<body></body>");;

(* attribute placeholders *)

Buffer.clear b;;

let t = Web.Template.parse_string "<TAG ?ATTR/>" in (* no space *)
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "ATTR" ->
			assert (Web.Template.is_empty subt && Web.Template.kind subt = `attribute);
			Web.Template.output_template print_substring subt;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "<TAG/>");;

Buffer.clear b;;

let t = Web.Template.parse_string "<TAG ?ATTR />" in (* space before "/>" *)
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "ATTR" ->
			assert (Web.Template.is_empty subt && Web.Template.kind subt = `attribute);
			Web.Template.output_template print_substring subt;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "<TAG/>");;

Buffer.clear b;;

let t = Web.Template.parse_string "<TAG ?ATTR ATTR=\"DUMMY\"/>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "ATTR" ->
			assert (
				not (Web.Template.is_empty subt) && Web.Template.kind subt = `attribute
			);
			let ac = Web.HTML.open_attribute `xhtml1 print_substring "INSERTED" in
			Web.HTML.attribute_output_string ac "VALUE";
			Web.HTML.close_attribute ac;
			Web.Template.output_template print_substring subt;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 1);
assert (Buffer.contents b = "<TAG INSERTED=\"VALUE\" ATTR=\"DUMMY\"/>");;

Buffer.clear b;;

let t = Web.Template.parse_string "<TAG ?A1 ?A2/>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
let acc =
	Web.Template.output_template_with (fun tag acc print_substring subt ->
		match tag with
		| "A1" | "A2" ->
			assert (Web.Template.is_empty subt && Web.Template.kind subt = `attribute);
			let ac = Web.HTML.open_attribute `xhtml1 print_substring tag in
			Web.HTML.attribute_output_string ac "VALUE";
			Web.HTML.close_attribute ac;
			acc + 1
		| _ ->
			Web.Template.unhandled tag
	) 0 (Buffer.add_substring b) t
in
assert (acc = 2);
assert (Buffer.contents b = "<TAG A1=\"VALUE\" A2=\"VALUE\"/>");;

Buffer.clear b;;

let t = Web.Template.parse_string "<TAG ? IGNORED/>" in
assert (not (Web.Template.is_empty t) && Web.Template.kind t = `element);
Web.Template.output_template (Buffer.add_substring b) t;
assert (Buffer.contents b = "<TAG/>");;

(* error cases *)

match Web.Template.parse_string "The </?closing> tag." with
| (_: Web.Template.template) -> assert false
| exception Web.Template.Parse_failure (pos, message) ->
	assert (pos = 4); (* '<' *)
	assert (message = "extra </?");;

(* marshaling *)

let s = "<?T1>Text<TAG ?A1/>node.</?T1>" in
let t = Web.Template.parse_string s in
let saved_t = Marshal.to_string t [] in
(* Printf.printf "saved template size = %d\n%!" (String.length saved_t); *)
assert (Marshal.from_string saved_t 0 = t);
let info, s' = (t :> Web.Template.parsed_info * string) in
assert (s' == s);
let saved_info = Marshal.to_string info [] in
(* Printf.printf "saved info size = %d\n%!" (String.length saved_info); *)
let restored_info = Marshal.from_string saved_info 0 in
assert (Web.Template.repair (restored_info, s) = t);
let s'' = Bytes.to_string (Bytes.copy (Bytes.of_string s)) in
assert (Web.Template.repair (restored_info, s'') = t);;

prerr_endline "ok";;
