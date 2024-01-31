open Location
open Freer

module BaseChoreo : sig 
  type _ t =
  | Local : ('a, 'b) locVal -> ('a * 'b) t
  | Trans : ('a, 'b) locVal -> ('a * 'c) t
end = struct
  type _ t =
  | Local : ('a, 'b) locVal -> ('a * 'b) t
  | Trans : ('a, 'b) locVal -> ('a * 'c) t
end

module Choreo = Freer.Make (BaseChoreo)

let locally x = Choreo.toFreer x
let _ = BaseChoreo.Local (wrap 1) |> locally

let example1 =
  Choreo.pure 1

let example2 =
  Choreo.pure 2
  
(* module Handler : Freer.HandlerS = struct
  type 'a boxt = BaseChoreo.t
  type 'a mont = 
  (* let handler : 'a boxt -> 'a mont *)
end *)
