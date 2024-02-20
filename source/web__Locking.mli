exception Access_failure of exn

val lockfile: string -> (unit -> 'a) -> 'a

val try_lockfile: string -> (unit -> 'a) -> ('a, exn) result

exception Timeout of exn

val timed_lockfile: timeout:int -> string -> (unit -> 'a) -> 'a
