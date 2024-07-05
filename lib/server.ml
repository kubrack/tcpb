open Unix
open Printf

let backlog = 10
let log_ch = open_out "log"
let log line = fprintf log_ch "%s\n%!" line

let listen_sock host port =
  let ip_addr = (gethostbyname host).h_addr_list.(0) in
  let sock_addr = ADDR_INET (ip_addr, port) in
  let listen_sock = socket PF_INET SOCK_STREAM 0 in
  let () = bind listen_sock sock_addr in
  let () = listen listen_sock backlog in
  let () =
    eprintf "srv : listening at %s:%d\n%!" (string_of_inet_addr ip_addr) port
  in
  listen_sock

let int_of_file_descr : Unix.file_descr -> int = Obj.magic (* FIXME *)

let add_client fd _out_chan =
  eprintf "srv : adding fd %d\n%!" (int_of_file_descr fd)

let rm_client ex fd =
  match ex with
  | Unix_error (unix_err, _syscall, _where) ->
      let () =
        eprintf "srv : closing %d: %s\n%!" (int_of_file_descr fd)
          (error_message unix_err)
      in
      let () = close fd in
      Lwt.return_none
  | _ -> raise ex

let p_accept lsocket = Lwt_unix.accept (Lwt_unix.of_unix_file_descr lsocket)

let start host port =
  let lsocket = listen_sock host port in
  let rec cb_accept (fd, addr) =
    let unix_fd = Lwt_unix.unix_file_descr fd in
    let msg =
      match addr with
      | ADDR_INET (ip, p) -> string_of_inet_addr ip ^ ":" ^ string_of_int p
      | ADDR_UNIX s -> s
    in
    let _ =
      log
        (sprintf "client restarted, accepted from %s as fd %d" msg
           (int_of_file_descr unix_fd))
    in
    let in_chan = Lwt_io.of_unix_fd ~mode:Lwt_io.Input unix_fd in
    let out_chan = Lwt_io.of_unix_fd ~mode:Lwt_io.Output unix_fd in
    let () = add_client unix_fd out_chan in
    let p_read () = Lwt_io.read_line in_chan in
    let rec cb_read line =
      let _ = log line in
      Lwt.try_bind p_read cb_read (fun ex -> rm_client ex unix_fd)
    in
    let _ = Lwt.try_bind p_read cb_read (fun ex -> rm_client ex unix_fd) in
    Lwt.bind (p_accept lsocket) cb_accept
  in
  Lwt.bind (p_accept lsocket) cb_accept
