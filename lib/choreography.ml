open Location
open Freer

module BaseChoreo : sig
  type _ t =
    | Var : 'locA locMod * 'a -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t
    | Comm : 'locB locMod * ('a, 'locA) locVal -> ('a, 'locB) locVal t
end = struct
  type _ t =
    | Var : 'locA locMod * 'a -> ('a, 'locA) locVal t
    | App :
        ('a -> 'b, 'locA) locVal * ('a, 'locA) locVal
        -> ('b, 'locA) locVal t
    | Comm : 'locB locMod * ('a, 'locA) locVal -> ('a, 'locB) locVal t
end

module Choreo = Freer.Make (BaseChoreo)

let var loc x = Choreo.toFreer (BaseChoreo.Var (loc, x))
let app f x = Choreo.toFreer (BaseChoreo.App (f, x))
let comm dest x = Choreo.toFreer (BaseChoreo.Comm (dest, x))
