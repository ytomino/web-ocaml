(** HTML generator.
    
    The contents of the template can be output while the placeholders replaced.
    {ul
    {- Tag placeholder: {v <?TAG>...</?TAG> v} }
    {- Attribute placeholder: {v <TAG ?ATTRIBUTE .../> v} }} *)

type parsed_info
type template = private parsed_info * string

val parsed_info_revision: int
(** The revision number of the representation of [parsed_info].
    It should be checked before unmarshaling [parsed_info]. *)

(** {1 Parsing} *)

exception Parse_failure of int * string

val parse_substring: string -> int -> int -> template
val parse_string: string -> template
(** Parse a string to template.
    @raise Parse_failure [(positon, message)] *)

(** {1 Marshaling} *)

val repair: parsed_info * string -> template
(** A template can be marshaled separately as parsed_info and a source string.
    [repair (p, s)] checks [p] is a result parsed from [s] and coercions its
    type to restore the template. *)

(** {1 Container operations} *)

val is_empty: template -> bool
(** Test whether a template is empty or not. *)

val kind: template -> [`element | `attribute]
(** Returns the kind of a template. *)

val find_opt: template -> string -> template option
(** [find_opt tag template] returns [Some subtemplate] named [tag] of
    [template], or [None] if [tag] does not exist. *)

val find: template -> string -> template
(** [find tag template] returns the subtemplate named [tag] of [template].
    @raise Not_found if [tag] does not exist *)

(** {1 Output} *)

exception Unhandled_tag of string

val unhandled: string -> 'a
(** The dummy handler for [output_template_with] and [output_subtemplate_with].
    @raise Unhandled_tag [tag] by [unhandled tag ...] *)

exception Not_found_tag of string

val output_template_with:
	(string -> 'a -> (string -> int -> int -> unit) -> template -> 'a) ->
	'a -> (string -> int -> int -> unit) -> template -> 'a
(** [output_template_with f acc print_substring template] outputs the contents
    of [template] with [print_substring].
    [f] is callbacked with [acc] for each subtemplate. *)

val output_subtemplate_with:
	(string -> 'a -> (string -> int -> int -> unit) -> template -> 'a) ->
	'a -> (string -> int -> int -> unit) -> template -> string -> 'a
(** [output_subtemplate_with f acc print_substring template tag] roughly does
    [output_template_with f acc print_substring (find template tag)].
    @raise Not_found_tag [tag] instead of [Not_found] *)

val output_template: (string -> int -> int -> unit) -> template -> unit
(** No handler variation of [output_template_with].
    If a template has subtemplate, it applys [unhandled]. *)

val output_subtemplate: (string -> int -> int -> unit) -> template -> string ->
	unit
(** No handler variation of [output_subtemplate_with]. *)
