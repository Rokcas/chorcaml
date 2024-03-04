open Chorcaml.Choreography
open Chorcaml.Location
open Lwt.Syntax

(* TODO: turn this module creation into a macro *)
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

let _ =
  print_endline "Please enter the location to execute the choreography for (a or b)";
  let loc = read_line () in
  let interpretation =
    match loc with
    | "a" -> Chorcaml.Handlers.Tcp.epp chor lmA
    | "b" -> Chorcaml.Handlers.Tcp.epp chor lmB
    | _ -> failwith "This location does not exist"
  in
  let prog =
    let* res = interpretation in
    let output =
      match res with None -> "None" | Some x -> string_of_int (unwrap_val x)
    in
    Lwt_io.printl output
  in
  Lwt_main.run prog
