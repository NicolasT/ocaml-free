module type Applicative = sig
    include Free_functor.Functor

    val pure : 'a -> 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
end
