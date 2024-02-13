open Chorcaml.Choreography
open Chorcaml.Functional
open Chorcaml.Location

(* TODO: turn this module creation into a macro *)
type locA

module LocA : LocS with type t = locA and type 'a mont = 'a Identity.t = struct
  type t = locA
  type 'a mont = 'a Identity.t
  type locUuid = int
  let uuid = 0
end

type locB

module LocB : LocS with type t = locB = struct
  type t = locB
  type 'a mont = 'a Identity.t
  type locUuid = int
  let uuid = 1
end

let lmA = (module LocA : LocS with type t = locA)
let lmB = (module LocB : LocS with type t = locB)


let _ =
  let chor1 =
    let open Choreo in
    let* x = 1 |> var lmA in
    let* y = 1 |> var lmA in
    let* add = ( + ) |> var lmA in
    let* part = app add x in
    app part y
  in
  let open Identity in
  Interpreter.interpFreer chor1 |> fmap (fun x -> unwrap_val x |> print_int)

let _ =
  let chor2 =
    let open Choreo in
    let* x = 0 |> var lmA in
    let* x' = x |> comm lmB in
    let inc x = x + 1 in
    let* f = inc |> var lmB in
    app f x'
  in
  let open Identity in
  Interpreter.interpFreer chor2 |> fmap (fun x -> unwrap_val x |> print_int)
