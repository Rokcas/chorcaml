module type FunctorS = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type ApplicativeS = sig
  type 'a t

  val return : 'a -> 'a t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module type MonadS = sig
  include FunctorS
  include ApplicativeS with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Identity : sig
  include MonadS

  val unwrap : 'a t -> 'a
end = struct
  type 'a t = Id of 'a

  let map f (Id x) = Id (f x)
  let return x = Id x
  let ( <*> ) (Id f) (Id x) = Id (f x)
  let ( >>= ) (Id x) f = f x
  let unwrap (Id x) = x
end
