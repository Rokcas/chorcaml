open Chorcaml
open Lwt.Syntax
open Location
open Choreography

type locA

module LocA : LocS with type t = locA = struct
  type t = locA
  type locUuid = int

  let uuid = 9000
end

type locB

module LocB : LocS with type t = locB = struct
  type t = locB
  type locUuid = int

  let uuid = 9001
end

let lmA = (module LocA : LocS with type t = locA)
let lmB = (module LocB : LocS with type t = locB)

let chor =
  let open Choreo in
  let* x = 0 |> var lmA in
  let* x' = x |> comm lmB in
  let inc x = x + 1 in
  let* f = inc |> var lmB in
  app f x'

let test_tcp_interpreter _ () =
  let thread_a =
    Handlers.Tcp.epp chor lmA |> Handlers.Tcp.MaybeLwt.map unwrap_val
  in
  let thread_b =
    Handlers.Tcp.epp chor lmB |> Handlers.Tcp.MaybeLwt.map unwrap_val
  in
  let* res_a, res_b = Lwt.both thread_a thread_b in
  let () = Alcotest.(check (option int)) "None at location A" None res_a in
  Lwt.return (Alcotest.(check (option int)) "None at location A" (Some 1) res_b)

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "TCP"
       [
         ( "all",
           [
             Alcotest_lwt.test_case "TCP interpreter" `Quick
               test_tcp_interpreter;
           ] );
       ]
