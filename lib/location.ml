module type LocS = sig
  type t
  type 'a mont
  type locUuid = int

  val uuid : locUuid
  (* val send : 'a -> locUuid -> unit mont
  val recv : locUuid -> 'a mont *)
end

(* 'b determines the location of the value *)
type 'a locMod = (module LocS with type t = 'a)
type ('a, 'b) locVal = LocVal of 'a * 'b locMod

let wrap (x : 'a) (l : (module LocS with type t = 'b)) : ('a, 'b) locVal =
  LocVal (x, l)

let unwrap_val (LocVal (x, _)) = x
let unwrap_loc (LocVal (_, m)) = m
