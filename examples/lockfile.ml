(* Demonstration for using Web.Locking. *)

Sys.catch_break true;;

let now () = Web.encode_date (Unix.time ());;

let pid = Unix.getpid ();;
let self = Sys.argv.(0);;

let timeout = 5;; (* sec. *)
let sleeptime = 10;;

let lockfilename =
	Filename.concat (Filename.get_temp_dir_name ()) "lockfile_sample"
in
Printf.printf "%d %s: lock file: %s\n" pid self lockfilename;
Printf.printf "%d %s: try at %s\n" pid self (now ());
flush stdout;
begin try
	Web.Locking.timed_lockfile ~timeout lockfilename (fun () ->
		Printf.printf "%d %s: begin at %s\n" pid self (now ());
		flush stdout;
		Unix.sleep sleeptime;
		Printf.printf "%d %s: end at %s\n" pid self (now ());
		flush stdout
	)
with
| Web.Locking.Timeout _ ->
	Printf.printf "%d %s: timeout at %s\n" pid self (now ())
| Web.Locking.Access_failure _ ->
	Printf.printf "%d %s: failure to open.\n" pid self
| Unix.Unix_error ((Unix.EAGAIN | Unix.EINTR), loc, _) ->
	Printf.printf "%d %s: failure to wait: %s\n" pid self loc
| Unix.Unix_error (_, loc, _) ->
	Printf.printf "%d %s: failure as other reason: %s.\n" pid self loc
end;;

prerr_endline "ok";;
