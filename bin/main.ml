let host = "127.0.0.1"
let port = 54321
let pcount = Sys.argv.(1) |> int_of_string

let client_exe =
  let dir = Filename.dirname Sys.executable_name in
  Filename.concat dir "client.exe"

let spawn () =
  match Unix.fork () with
  | 0 -> Unix.execv client_exe [| client_exe; host; string_of_int port |]
  | _pid -> ()

let rec spawn_all pcount =
  match pcount with
  | 0 -> ()
  | c ->
      let () = spawn () in
      spawn_all (c - 1)

let rec cb_wait (_pid, _status) =
  let () = spawn () in
  Lwt.bind (Lwt_unix.wait ()) cb_wait

let rec wait_loop () =
  let _ = Lwt_main.run (Lwt.bind (Lwt_unix.wait ()) cb_wait) in
  wait_loop ()

let _ = Tcpb.Server.start host port
let () = spawn_all pcount
let () = wait_loop ()
