open Location
open Freer

module BaseChoreo : sig
  type _ t =
    | Var : 'a * 'locA locMod -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t
    | Comm : ('a, 'locA) locVal * 'locB locMod -> ('a, 'locB) locVal t
end = struct
  type _ t =
    | Var : 'a * 'locA locMod -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t
    | Comm : ('a, 'locA) locVal * 'locB locMod -> ('a, 'locB) locVal t
end

module Choreo = Freer.Make (BaseChoreo)

module Handler :
  Freer.HandlerS
    with type 'a boxt = 'a BaseChoreo.t
     and type 'a mont = 'a Functional.Identity.t = struct
  type 'a boxt = 'a BaseChoreo.t
  type 'a mont = 'a Functional.Identity.t

  include Functional.Identity

  let handler : type a. a boxt -> a mont =
   fun x ->
    let open BaseChoreo in
    match x with
    | Var (v, l) -> pure (wrap v l)
    | App (f, v) -> pure (wrap ((unwrap_val f) (unwrap_val v)) (unwrap_loc f))
    | Comm (v, l) -> pure (wrap (unwrap_val v) l)
end

module Interpreter = Freer.MakeInterp (Choreo) (Handler)

let var loc x = Choreo.toFreer (BaseChoreo.Var (x, loc))
let app f x = Choreo.toFreer (BaseChoreo.App (f, x))
let comm dest x = Choreo.toFreer (BaseChoreo.Comm (x, dest))
