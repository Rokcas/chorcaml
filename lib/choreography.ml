open Location
open Freer

module BaseChoreo : sig
  type _ t =
    | Var : 'a * 'locA locMod -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t (* No need for locMod? *)
    | Comm :
        ('a, 'locA) locVal * 'locA locMod * 'locB locMod
        -> ('a, 'locB) locVal t
end = struct
  type _ t =
    | Var : 'a * 'locA locMod -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t (* No need for locMod? *)
    | Comm :
        ('a, 'locA) locVal * 'locA locMod * 'locB locMod
        -> ('a, 'locB) locVal t
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
    match x with
    | Var (v, _) -> pure (wrap v)
    | App (f, v) -> pure (wrap ((unwrap f) (unwrap v)))
    | Comm (v, _, _) -> pure (wrap (unwrap v))
end

module Interpreter = Freer.MakeInterp (Choreo) (Handler)

let var x modA = Choreo.toFreer (BaseChoreo.Var (x, modA))
let app f x = Choreo.toFreer (BaseChoreo.App (f, x))
let comm x modA modB = Choreo.toFreer (BaseChoreo.Comm (x, modA, modB))
