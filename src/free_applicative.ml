module type Applicative = sig
    include Free_functor.Functor

    val pure : 'a -> 'a t
    val ap : ('a -> 'b) t -> 'a t -> 'b t
end

module Infix(A : Applicative) : sig
    val (<$>) : ('a -> 'b) -> 'a A.t -> 'b A.t
    val (<*>) : ('a -> 'b) A.t -> 'a A.t -> 'b A.t
end = struct
    module F = Free_functor.Infix(A)
    include F

    let (<*>) = A.ap
end

module Utils(A : Applicative) : sig
    module Infix : sig
        val (<$>) : ('a -> 'b) -> 'a A.t -> 'b A.t
        val (<$) : 'a -> 'b A.t -> 'a A.t

        val (<*>) : ('a -> 'b) A.t -> 'a A.t -> 'b A.t
        val (<**>) : 'a A.t -> ('a -> 'b) A.t -> 'b A.t

        val ( *>) : 'a A.t -> 'b A.t -> 'b A.t
        val (<*) : 'a A.t -> 'b A.t -> 'a A.t
    end

    val map : ('a -> 'b) -> 'a A.t -> 'b A.t

    val pure : 'a -> 'a A.t
    val ap : ('a -> 'b) A.t -> 'a A.t -> 'b A.t

    val liftA : ('a -> 'b) -> 'a A.t -> 'b A.t
    val liftA2 : ('a -> 'b -> 'c) -> 'a A.t -> 'b A.t -> 'c A.t
    val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a A.t -> 'b A.t -> 'c A.t -> 'd A.t
end = struct
    include A

    module F = Free_functor.Utils(A)

    module Infix = struct
        include F.Infix
        module AI = Infix(A)
        include AI

        let (<**>) a f = f <*> a

        let ( *>) a b = (fun _ b -> b) <$> a <*> b
        let (<*) a b = (fun a _ -> a) <$> a <*> b
    end

    open Infix

    let liftA = A.map
    let liftA2 f a b = liftA f a <*> b
    let liftA3 f a b c = liftA2 f a b <*> c
end
