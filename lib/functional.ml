module type FunctorS = sig
  type 'a t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type ApplicativeS = sig
  type 'a t

  val pure : 'a -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module type MonadS = sig
  include FunctorS
  include ApplicativeS with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Identity : MonadS = struct
  type 'a t = Id of 'a

  let fmap f (Id x) = Id (f x)
  let pure x = Id x
  let ( <*> ) (Id f) (Id x) = Id (f x)
  let ( >>= ) (Id x) f = f x
end
