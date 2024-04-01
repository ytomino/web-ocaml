type node = {
	contents_pos: int;
	contents_len: int;
	tag_pos: int;
	tag_len: int;
	subtemplate: parsed_info
}
and parsed_info = {
	nodes: node list;
	rem_contents_pos: int;
	rem_contents_len: int;
	kind: [`element | `attribute];
	source_hash: int
};;

type template = parsed_info * string;;

let parsed_info_revision = 0x100;; (* increment when parsed_info is changed *)

exception Parse_failure of int * string;;

let unsafe_parse_substring: string -> int -> int -> template =
	let rec skip_spaces s pos end_pos = (
		if pos >= end_pos
			|| (
				match s.[pos] with
				| ' ' | '\t' | '\n' | '\r' -> false
				| _ -> true
			)
		then pos
		else skip_spaces s (pos + 1) end_pos
	) in
	let rec back_spaces s pos end_pos = (
		if pos >= end_pos
			|| (
				match s.[end_pos - 1] with
				| ' ' | '\t' | '\n' | '\r' -> false
				| _ -> true
			)
		then end_pos
		else back_spaces s pos (end_pos - 1)
	) in
	let get_closing s pos end_pos = (
		assert (pos < end_pos);
		if pos + 1 < end_pos && s.[pos] = '/' && s.[pos + 1] = '>' then pos + 2 else
		if pos + 1 < end_pos && s.[pos] = '?' && s.[pos + 1] = '>' then pos + 2 else
		if s.[pos] = '>' then pos + 1
		else pos
	) in
	let rec get_string s pos end_pos quote = (
		if pos >= end_pos then pos else
		if s.[pos] = quote then pos + 1
		else get_string s (pos + 1) end_pos quote
	) in
	let rec get_name s pos end_pos = (
		if pos >= end_pos
			|| (
				match s.[pos] with
				| ' ' | '\t' | '\n' | '\r' | '/' | '=' | '>' | '?' -> true
				| _ -> false
			)
		then pos
		else get_name s (pos + 1) end_pos
	) in
	let rec get_xml_declaration s pos end_pos = (
		if pos + 1 < end_pos && s.[pos] = '?' && s.[pos + 1] = '>' then pos + 2 else
		if pos < end_pos then (
			let pos =
				match s.[pos] with
				| '"' | '\'' as quote -> get_string s (pos + 1) end_pos quote
				| _ -> pos + 1
			in
			get_xml_declaration s pos end_pos
		) else raise (Parse_failure (pos, "expect >"))
	) in
	let rec get_attribute_ph_contents s pos end_pos = (
		if pos < end_pos then (
			let closed_pos = get_closing s pos end_pos in
			if closed_pos > pos then pos, closed_pos else
			if s.[pos] = '?' then pos, pos
			else get_attribute_ph_contents s (pos + 1) end_pos
		) else raise (Parse_failure (pos, "expect >"))
	) in
	let rec parse_attribute_ph s pos end_pos ~source_hash result contents_pos
		contents_len =
	(
		(* <TAG ... ?ATTRIBUTE ... > *)
		assert (s.[pos] = '?');
		let tag_pos = pos + 1 in (* ? *)
		let pos = get_name s tag_pos end_pos in
		let tag_len = pos - tag_pos in
		let rem_contents_pos = pos in
		let pos, closed_pos = get_attribute_ph_contents s pos end_pos in
		(* Trim spaces before next attribute placeholder. *)
		let rem_contents_len = back_spaces s rem_contents_pos pos - rem_contents_pos in
		let result =
			{
				contents_pos;
				contents_len;
				tag_pos;
				tag_len;
				subtemplate = {
					nodes = [];
					rem_contents_pos;
					rem_contents_len;
					kind = `attribute;
					source_hash
				}
			} :: result
		in
		if closed_pos > pos
		then parse_nodes s closed_pos end_pos ~source_hash result pos
		else (
			assert (s.[pos] = '?');
			parse_attribute_ph s pos end_pos ~source_hash result pos 0
		)
	) and parse_inside_tag s pos end_pos ~source_hash result contents_pos = (
		if pos < end_pos then (
			let closed_pos = get_closing s pos end_pos in
			if closed_pos > pos
			then parse_nodes s closed_pos end_pos ~source_hash result contents_pos
			else
			match s.[pos] with
			| '?' ->
				(* Trim spaces before attribute placeholder. *)
				let contents_len = back_spaces s contents_pos pos - contents_pos in
				parse_attribute_ph s pos end_pos ~source_hash result contents_pos contents_len
			| '"' | '\'' as quote ->
				let pos = get_string s (pos + 1) end_pos quote in
				parse_inside_tag s pos end_pos ~source_hash result contents_pos
			| _ ->
				parse_inside_tag s (pos + 1) end_pos ~source_hash result contents_pos
		) else raise (Parse_failure (pos, "expect >"))
	) and parse_nodes s pos end_pos ~source_hash result contents_pos = (
		match
			if pos >= end_pos then `exit else
			if s.[pos] <> '<' then `skip else
			if pos + 2 < end_pos && s.[pos + 1] = '/' && s.[pos + 2] = '?' then `exit else
			if pos + 1 < end_pos && s.[pos + 1] = '/' then `skip else
			if pos + 1 < end_pos && s.[pos + 1] = '?' then `lt_q
			else `lt
		with
		| `exit ->
			List.rev result, contents_pos, pos
		| `skip ->
			parse_nodes s (pos + 1) end_pos ~source_hash result contents_pos
		| `lt_q ->
			(* Trim spaces before start-tag placeholder. *)
			let contents_len = back_spaces s contents_pos pos - contents_pos in
			assert (s.[pos] = '<' && s.[pos + 1] = '?');
			let tag_pos = pos + 2 in (* <? *)
			let pos = get_name s tag_pos end_pos in
			let tag_len = pos - tag_pos in
			let pos = skip_spaces s pos end_pos in
			begin match
				if pos + 1 < end_pos && s.[pos] = '/' && s.[pos + 1] = '>' then `empty else
				if pos < end_pos && s.[pos] = '>' then `container
				else `expect_gt
			with
			| `container | `empty as kind ->
				let pos, subtemplate =
					match kind with
					| `container ->
						(* <?TAG>...</?TAG> *)
						assert (s.[pos] = '>');
						let pos = pos + 1 in (* > *)
						let subtemplate, endtag_pos = parse_info s pos end_pos source_hash in
						let tag = String.sub s tag_pos tag_len in
						if endtag_pos + 3 + tag_len < end_pos && s.[endtag_pos] = '<'
							&& s.[endtag_pos + 1] = '/' && s.[endtag_pos + 2] = '?'
							&& (
								let endtag_name_pos = endtag_pos + 3 in (* </? *)
								let endtag_name_len = get_name s endtag_name_pos end_pos - endtag_name_pos in
								endtag_name_len = tag_len && String.sub s endtag_name_pos endtag_name_len = tag
							) && s.[endtag_pos + 3 + tag_len] = '>'
						then (
							let pos = endtag_pos + 3 + tag_len + 1 in (* </?TAG> *)
							pos, subtemplate
						) else raise (Parse_failure (pos, "expect </?" ^ tag ^ ">"))
					| `empty ->
						(* <?TAG/> *)
						assert (s.[pos] = '/' && s.[pos + 1] = '>');
						let pos = pos + 2 in (* /> *)
						let subtemplate = {
							nodes = [];
							rem_contents_pos = pos;
							rem_contents_len = 0;
							kind = `element;
							source_hash
						}
						in
						pos, subtemplate
				in
				let result =
					{
						contents_pos;
						contents_len;
						tag_pos;
						tag_len;
						subtemplate
					} :: result
				in
				parse_nodes s pos end_pos ~source_hash result pos
			| `expect_gt ->
				(* <? ... ?> like XML declaration *)
				let pos = get_xml_declaration s pos end_pos in
				parse_nodes s pos end_pos ~source_hash result contents_pos
			end
		| `lt ->
			parse_inside_tag s (pos + 1) end_pos ~source_hash result contents_pos
	) and parse_info s pos end_pos source_hash = (
		let nodes, rem_contents_pos, endtag_pos =
			parse_nodes s pos end_pos ~source_hash [] pos
		in
		(* Trim spaces before end-tag placeholder. *)
		let rem_contents_len =
			(
				if endtag_pos = end_pos then endtag_pos
				else back_spaces s pos endtag_pos
			) - rem_contents_pos
		in
		{
			nodes;
			rem_contents_pos;
			rem_contents_len;
			kind = `element;
			source_hash
		}, endtag_pos
	) in
	fun s pos len ->
	let source_hash = Hashtbl.hash s in
	let end_pos = pos + len in
	let info, endtag_pos = parse_info s pos end_pos source_hash in
	if endtag_pos = end_pos then info, s
	else raise (Parse_failure (endtag_pos, "extra </?"));;

let parse_substring (s: string) (pos: int) (len: int) = (
	if pos >= 0 && len >= 0 && len <= String.length s - pos
	then unsafe_parse_substring s pos len
	else invalid_arg "Web.HTML.text_output_substring" (* __FUNCTION__ *)
);;

let parse_string (s: string) = (
	unsafe_parse_substring s 0 (String.length s)
);;

let repair (template: template) = (
	let {source_hash; _}, s = template in
	if source_hash = Hashtbl.hash s then template
	else failwith "Web.Template.repair" (* __FUNCTION__ *)
);;

let is_empty (info, _: parsed_info * string) = (
	let {nodes; rem_contents_len; _} = info in
	nodes = [] && rem_contents_len = 0
);;

let kind ({kind; _}, _: parsed_info * string) = kind;;

let find_opt (info, s: parsed_info * string) (tag: string) = (
	let {nodes; _} = info in
	List.find_map (fun node ->
		let {tag_pos; tag_len; subtemplate; _} = node in
		if String.sub s tag_pos tag_len = tag then Some (subtemplate, s)
		else None
	) nodes
);;

let find (template: template) (tag: string) = (
	match find_opt template tag with
	| None -> raise Not_found
	| Some subtemplate -> subtemplate
);;

exception Unhandled_tag of string

let unhandled (tag: string) = raise (Unhandled_tag tag);;

exception Not_found_tag of string

let output_template_with: type a.
	(string -> a -> (string -> int -> int -> unit) -> template -> a) ->
	a -> (string -> int -> int -> unit) -> template -> a =
	let rec loop f a print_substring template nodes = (
		let info, s = template in
		match nodes with
		| [] ->
			let {rem_contents_pos; rem_contents_len; _} = info in
			if rem_contents_len > 0 then (
				print_substring s rem_contents_pos rem_contents_len
			);
			a
		| node :: tl ->
			let {contents_pos; contents_len; tag_pos; tag_len; subtemplate} = node in
			if contents_len > 0 then (
				print_substring s contents_pos contents_len
			);
			let a =
				if tag_len > 0 then (
					let tag = String.sub s tag_pos tag_len in
					f tag a print_substring (subtemplate, s)
				) else a
			in
			loop f a print_substring template tl
	) in
	fun f a print_substring template ->
	let {nodes; _}, _ = template in
	loop f a print_substring template nodes;;
(* Note, List.fold_left can be used here, but it will cause noise in
   backtracing. *)

let output_subtemplate_with (type a)
	(f: string -> a -> (string -> int -> int -> unit) -> template -> a)
	(a: a) (print_substring: string -> int -> int -> unit) (template: template)
	(tag: string) =
(
	match find_opt template tag with
	| None -> raise (Not_found_tag tag)
	| Some subtemplate -> output_template_with f a print_substring subtemplate
);;

let output_template: (string -> int -> int -> unit) -> template -> unit =
	output_template_with unhandled ();;

let output_subtemplate: (string -> int -> int -> unit) -> template -> string ->
	unit =
	output_subtemplate_with unhandled ();;
