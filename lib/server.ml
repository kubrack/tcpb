open Unix
open Printf

let backlog = 64
let log_ch = open_out "log"
let log line = fprintf log_ch "%s\n%!" line

let listen_sock host port =
  let ip_addr = (gethostbyname host).h_addr_list.(0) in
  let sock_addr = ADDR_INET (ip_addr, port) in
  let listen_sock = socket PF_INET SOCK_STREAM 0 in
  let () = bind listen_sock sock_addr in
  let () = listen listen_sock backlog in
  listen_sock

let rm_client ex fd =
  match ex with
  | Unix_error (_unix_err, _syscall, _where) ->
      let () = close fd in
      Lwt.return_none
  | _ -> raise ex

let client_out_channels = ref []

let add_ch fd =
  let out_chan = out_channel_of_descr fd in
  client_out_channels := out_chan :: !client_out_channels

let p_stdin () = Lwt_io.read_line (Lwt_io.of_unix_fd ~mode:Lwt_io.Input stdin)

let rec cb_stdin line =
  let write chan =
    try (fprintf chan "%s\n%!" line; Some chan)
    with _ex -> (close (descr_of_out_channel chan); None) in
  client_out_channels := List.filter_map write !client_out_channels;
  Lwt.bind (p_stdin ()) cb_stdin

let p_accept lsocket = Lwt_unix.accept (Lwt_unix.of_unix_file_descr lsocket)

let () = Sys.set_signal Sys.sigpipe (Sys.Signal_ignore)

let start host port =
  let lsocket = listen_sock host port in
  let rec cb_accept (fd, addr) =
    let unix_fd = Lwt_unix.unix_file_descr fd in
    let msg =
      match addr with
      | ADDR_INET (ip, p) -> string_of_inet_addr ip ^ ":" ^ string_of_int p
      | ADDR_UNIX s -> s
    in
    let _ = log (sprintf "%s spawned." msg) in
    let in_chan = Lwt_io.of_unix_fd ~mode:Lwt_io.Input unix_fd in
    let () = add_ch unix_fd in
    let p_read () = Lwt_io.read_line in_chan in
    let rec cb_read line =
      let _ = log line in
      Lwt.try_bind p_read cb_read (fun ex -> rm_client ex unix_fd)
    in
    let _ = Lwt.try_bind p_read cb_read (fun ex -> rm_client ex unix_fd) in
    Lwt.bind (p_accept lsocket) cb_accept
  in
  let _ = Lwt.bind (p_stdin ()) cb_stdin in
  Lwt.bind (p_accept lsocket) cb_accept
