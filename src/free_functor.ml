module type Functor = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
end

module Infix(F : Functor) : sig
    val (<$>) : ('a -> 'b) -> 'a F.t -> 'b F.t
end = struct
    let (<$>) = F.map
end

module Utils(F : Functor) : sig
    module Infix : sig
        val (<$>) : ('a -> 'b) -> 'a F.t -> 'b F.t
        val (<$) : 'a -> 'b F.t -> 'a F.t
    end

    val map : ('a -> 'b) -> 'a F.t -> 'b F.t
end = struct
    include F

    module Infix = struct
        module M = Infix(F)
        include M

        let (<$) a f = F.map (fun _ -> a) f
    end

    let map = F.map
end
