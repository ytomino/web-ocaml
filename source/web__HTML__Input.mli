open Web
open Web__HTML;;

val output_map: version -> (string -> int -> int -> unit) -> ?endline:bool ->
	[`hidden] -> string StringMap.t -> unit
