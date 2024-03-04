open Functional

module Freer = struct
  module type S = sig
    type 'a boxt

    type _ freer =
      | Pure : 'a -> 'a freer
      | Impure : 'a boxt * ('a -> 'b freer) -> 'b freer

    include MonadS with type 'a t = 'a freer

    val impure : 'a boxt -> ('a -> 'b freer) -> 'b freer
    val toFreer : 'a boxt -> 'a freer
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Make (Box : sig
    type 'a t
  end) : S with type 'a boxt = 'a Box.t = struct
    type _ freer =
      | Pure : 'a -> 'a freer
      | Impure : 'a Box.t * ('a -> 'b freer) -> 'b freer

    type 'a t = 'a freer
    type 'a boxt = 'a Box.t

    let return a = Pure a
    let impure f k = Impure (f, k)
    let toFreer eff = impure eff return

    let rec map f fr =
      match fr with
      | Pure a -> return (f a)
      | Impure (eff, k) -> impure eff (fun x -> map f (k x))

    let rec ( <*> ) fr ap =
      match fr with
      | Pure a -> map a ap
      | Impure (eff, k) -> impure eff (fun x -> k x <*> ap)

    let rec ( >>= ) fr f =
      match fr with
      | Pure a -> f a
      | Impure (eff, k) -> impure eff (fun x -> k x >>= f)

    let ( let* ) = ( >>= )
  end

  module type HandlerS = sig
    type 'a boxt
    type 'a mont

    val handler : 'a boxt -> 'a mont

    include MonadS with type 'a t := 'a mont
  end

  module type InterpS = sig
    type 'a boxt
    type 'a mont
    type 'a t

    val interpFreer : 'a t -> 'a mont
  end

  module MakeInterp (Fr : S) (Hand : HandlerS with type 'a boxt = 'a Fr.boxt) :
    InterpS
      with type 'a t = 'a Fr.t
       and type 'a boxt = 'a Fr.boxt
       and type 'a mont = 'a Hand.mont = struct
    type 'a boxt = 'a Fr.boxt
    type 'a mont = 'a Hand.mont
    type 'a t = 'a Fr.t

    open Hand

    let rec interpFreer fr =
      match fr with
      | Fr.Pure a -> return a
      | Fr.Impure (eff, k) -> handler eff >>= fun x -> interpFreer (k x)
  end
end
