open Printf
open Unix

let keepalive_interval_from = 1. (* FIXME *)
let keepalive_interval_to = 2.
let kill_interval_from = 10.
let kill_interval_to = 20.

let float_in_range min max = Random.float (max -. min) +. min 
let keepalive_interval () = float_in_range keepalive_interval_from keepalive_interval_to
let kill_interval () = float_in_range kill_interval_from kill_interval_to

let mk_socket host port =
  let ip_addr = (gethostbyname host).h_addr_list.(0) in
  let sock_addr = ADDR_INET (ip_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let () = (* FIXME : remove debug*)
    eprintf "Connecting to %s:%d\n%!" (string_of_inet_addr ip_addr) port
  in
  let () = connect sock sock_addr in
  sock

let sock_id socket = 
  match getsockname socket with
  | ADDR_INET (ip, p) -> string_of_inet_addr ip ^ ":" ^ string_of_int p
  | ADDR_UNIX s -> s

let p_kill () = Lwt_unix.sleep (kill_interval ())
let cb_kill () = raise Exit
  
let run host port =
  let socket = mk_socket host port in
  let in_chan = Lwt_io.of_unix_fd ~mode:Lwt_io.Input socket in
  let out_chan  = Lwt_io.of_unix_fd ~mode:Lwt_io.Output socket in
  let sock_id = sock_id socket in

  let p_keepalive () = Lwt_unix.sleep (keepalive_interval ()) in
  let rec cb_keepalive () = begin
    let line = sock_id ^ " is alive" in
    let _ = Lwt_io.write_line out_chan line in
    Lwt.bind (p_keepalive ()) cb_keepalive
  end in

  let p_ack () = Lwt_io.read_line in_chan in
  let rec cb_ack line = begin
    let ack = sock_id ^ " ack: len=" ^ (string_of_int (String.length line)) in
    let _ = Lwt_io.write_line out_chan ack in
    Lwt.bind (p_ack ()) cb_ack
  end in

  let kill = Lwt.bind (p_kill ()) cb_kill in
  let _p_keepalive = Lwt.bind (p_keepalive ()) cb_keepalive in
  let _p_ack = Lwt.bind (p_ack ()) cb_ack in
  ignore (Lwt_main.run kill)
