module type FunctorS = sig
  type 'a t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type ApplicativeS = sig
  type 'a t
  val pure : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module type MonadS = sig
  include FunctorS
  include ApplicativeS with type 'a t := 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end