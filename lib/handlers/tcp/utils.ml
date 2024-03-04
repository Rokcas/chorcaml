open Lwt.Syntax

let create_socket port =
  (* Convenience function for creating a socket on localhost *)
  let open Lwt_unix in
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_REUSEADDR true;
  let addr = ADDR_INET (Unix.inet_addr_loopback, port) in
  let* () = bind sock addr in
  Lwt.return sock

let create_sender value sender_port receiver_port =
  (* Creates a server that listens for a connection from the receiver.
     The server terminates upon sending the value to the designated receiver.
     Any other clients attempting to connect will be sent no data.
  *)
  let open Lwt_unix in
  let* sock = create_socket sender_port in
  listen sock 5;
  let rec loop () =
    let* s, caller = accept sock in
    let correct_client =
      match caller with
      | ADDR_INET (_, port) when port == receiver_port -> true
      | _ -> false
    in
    if not correct_client then
      let* () = close s in
      loop ()
    else
      let out = Lwt_io.(of_fd ~mode:Output s) in
      let* () = Lwt_io.write_value out value in
      let* () = Lwt_io.flush out in
      let* () = close s in
      close sock
  in
  loop ()

let catch_sender_not_ready t f =
  (* Catches errors specific to the sender not being ready.
     EOF is raised when the sender is currently meant to transmit a result to another client.
     ECONNREFUSED is raised when the sender is not yet ready to send a value.
  *)
  let handler exn =
    match exn with
    | End_of_file | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> f ()
    | e -> Lwt.fail e
  in
  Lwt.catch t handler

let create_delay_generator initial_delay multiplier noise =
  (* Creates a function calculating exponentially increasing delay with random noise *)
  let base_delay = ref initial_delay in
  let get_delay () =
    let perturbation = (Random.float noise *. 2.0) -. noise in
    let delay = !base_delay *. (1.0 +. perturbation) in
    base_delay := !base_delay *. multiplier;
    delay
  in
  get_delay

let create_receiver receiver_port sender_port : 'a Lwt.t =
  (* Creates a client that tries to read a value from the server.
     The client retries until the server is ready to send it the value.
  *)
  let delay_fun = create_delay_generator 0.1 1.5 0.1 in
  let rec create_receiver' receiver_port sender_port =
    let open Lwt_unix in
    let* sock = create_socket receiver_port in
    let sender_addr = ADDR_INET (Unix.inet_addr_loopback, sender_port) in
    let receive () =
      let* () = connect sock sender_addr in
      let inp = Lwt_io.(of_fd ~mode:Input sock) in
      Lwt_io.read_value inp
    in
    catch_sender_not_ready
      (fun () -> Lwt.finalize receive (fun () -> close sock))
      (fun () ->
        let* () = delay_fun () |> sleep in
        create_receiver' receiver_port sender_port)
  in
  create_receiver' receiver_port sender_port
