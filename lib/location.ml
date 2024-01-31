module type LocS = sig
  type t
end

(* TODO: turn this module creation into a macro *)
type locA
module LocA : LocS = struct
  type t = locA
end

type locB
module LocB : LocS = struct
  type t = locA
end

(* 'b determines the location of the value *)
type ('a, 'b) constrVal = Val of 'a
type ('a, 'b) locVal = ('a, (module LocS with type t = 'b)) constrVal

let wrap (x: 'a) : ('a, 'b) locVal = Val x

let unwrap (Val x: ('a, 'b) locVal) : 'a = x