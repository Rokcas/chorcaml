open Freer
open Choreography
open Functional
open Location

module LocalHandler :
  Freer.HandlerS
    with type 'a boxt = 'a BaseChoreo.t
     and type 'a mont = 'a Identity.t = struct
  type 'a boxt = 'a BaseChoreo.t
  type 'a mont = 'a Identity.t

  include Identity

  let handler : type a. a boxt -> a mont =
   fun x ->
    let open BaseChoreo in
    match x with
    | Var (l, v) -> return (wrap v l)
    | App (f, v) -> return (wrap ((unwrap_val f) (unwrap_val v)) (unwrap_loc f))
    | Comm (l, v) -> return (wrap (unwrap_val v) l)
end

module LocalInterpreter = Freer.MakeInterp (Choreo) (LocalHandler)
