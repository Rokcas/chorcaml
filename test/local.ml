open Chorcaml
open Location
open Choreography
open Functional

type locA

module LocA : LocS with type t = locA = struct
  type t = locA
  type locUuid = int

  let uuid = 0
end

let lmA = (module LocA : LocS with type t = locA)

let chor =
  let open Choreo in
  let* x = 1 |> var lmA in
  let* y = 1 |> var lmA in
  let* add = ( + ) |> var lmA in
  let* part = app add x in
  app part y

let test_local_interpreter () =
  Alcotest.(check int)
    "same int" 2
    (Handlers.Local.LocalInterpreter.interpFreer chor
    |> Identity.unwrap |> unwrap_val)

let () =
  let open Alcotest in
  run "Local"
    [
      ("all", [ test_case "Basic interpretation" `Quick test_local_interpreter ]);
    ]
