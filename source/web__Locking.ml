exception Access_failure of exn;;

let open_file (filename: string) = (
	match
		Unix.openfile filename [Unix.O_CREAT; Unix.O_WRONLY; Unix.O_CLOEXEC] 0o600
	with
	| fd ->
		fd
	| exception (Unix.Unix_error _ as exn) ->
		raise (Access_failure exn)
);;

let lock (fd: Unix.file_descr) = (
	Unix.lockf fd Unix.F_LOCK 0
);;

let try_lock (fd: Unix.file_descr) = (
	match Unix.lockf fd Unix.F_TLOCK 0 with
	| () ->
		Ok ()
	| exception (Unix.Unix_error (Unix.EAGAIN, _, _) as exn) ->
		Error exn
);;

exception Timeout of exn;;

let timed_lock ~(timeout: int) (fd: Unix.file_descr) = (
	match try_lock fd with
	| Ok () ->
		()
	| Error exn ->
		if timeout > 0 then (
			let alarmed = ref false in
			let previous_sigalrm =
				Sys.signal Sys.sigalrm (
					Sys.Signal_handle (fun _ ->
						alarmed := true
					)
				)
			in
			Fun.protect ~finally:(fun () ->
				Sys.set_signal Sys.sigalrm previous_sigalrm
			) (fun () ->
				let previous_it =
					Unix.setitimer Unix.ITIMER_REAL {
						Unix.it_interval = 1.;
							(* Set an interval to avoid waiting indefinitely if fcntl returns EINTER for
							   other reasons and SIGALRM is arrived until fcntl is called again. *)
						Unix.it_value = float_of_int timeout
					}
				in
				let rec loop () = ( (* closed fd and alarmed *)
					try lock fd with
					| Unix.Unix_error (Unix.EINTR, _, _) as exn ->
						if !alarmed then raise (Timeout exn)
						else loop ()
				) in
				Fun.protect ~finally:(fun () ->
					let _: Unix.interval_timer_status =
						Unix.setitimer Unix.ITIMER_REAL previous_it
					in
					()
				) loop
			)
		) else raise (Timeout exn)
);;

let with_open_file (type a) (filename: string) (f: Unix.file_descr -> a) = (
	let fd = open_file filename in
	Fun.protect ~finally:(fun () ->
		Unix.close fd
	) (fun () ->
		f fd
	)
);;

let lockfile (type a) (filename: string) (f: unit -> a) = (
	with_open_file filename (fun fd ->
		lock fd;
		f ()
	)
);;

let try_lockfile (type a) (filename: string) (f: unit -> a) = (
	with_open_file filename (fun fd ->
		match try_lock fd with
		| Ok () -> Ok (f ())
		| Error _ as error -> error
	)
);;

let timed_lockfile (type a) ~(timeout: int) (filename: string) (f: unit -> a) =
(
	with_open_file filename (fun fd ->
		timed_lock ~timeout fd;
		f ()
	)
);;
