open Location
open Freer
open Functional

module BaseChoreo : sig
  type _ t =
    | Local : ('a, 'b) locVal -> ('a, 'b) locVal t
    | Trans : ('a, 'b) locVal -> ('a, 'c) locVal t
end = struct
  type _ t =
    | Local : ('a, 'b) locVal -> ('a, 'b) locVal t
    | Trans : ('a, 'b) locVal -> ('a, 'c) locVal t
end

module Choreo = Freer.Make (BaseChoreo)

module Handler :
  Freer.HandlerS
    with type 'a boxt = 'a BaseChoreo.t
     and type 'a mont = 'a Functional.Identity.t = struct
  type 'a boxt = 'a BaseChoreo.t
  type 'a mont = 'a Functional.Identity.t

  include Functional.Identity

  (* Handler does nothing interesting for now *)
  let handler : type a. a boxt -> a mont =
   fun x ->
    let open BaseChoreo in
    match x with Local v -> pure v | Trans v -> pure (wrap (unwrap v))
end

module Interpreter = Freer.MakeInterp (Choreo) (Handler)
