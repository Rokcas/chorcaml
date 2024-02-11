module type LocS = sig
  type t
end

(* 'b determines the location of the value *)
type ('a, 'b) constrVal = Val of 'a
type 'a locMod = (module LocS with type t = 'a)
type ('a, 'b) locVal = ('a, 'b locMod) constrVal

let wrap (x : 'a) : ('a, 'b) locVal = Val x
let unwrap (Val x : ('a, 'b) locVal) : 'a = x
