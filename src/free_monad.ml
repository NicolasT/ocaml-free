module type Monad = sig
    include Free_applicative.Applicative

    val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Make(F : Free_functor.Functor) : sig
    type 'a t = Pure of 'a
              | Free of ('a t) F.t

    include Monad with type 'a t := 'a t

    val lift : 'a F.t -> 'a t
    val iter : ('a F.t -> 'a) -> 'a t -> 'a

    module IterM(M : Monad) : sig
        val iterM : ('a M.t F.t -> 'a M.t) -> 'a t -> 'a M.t
    end
end = struct
    type 'a t = Pure of 'a
              | Free of ('a t) F.t

    let map f =
        let rec loop = function
          | Pure a -> Pure (f a)
          | Free fa -> Free (F.map loop fa)
        in
        loop

    let pure a = Pure a
    let ap =
        let rec loop a b = match a with
          | Pure a' -> begin match b with
              | Pure b' -> Pure (a' b')
              | Free b' -> Free (F.map (map a') b')
            end
          | Free a' -> Free (F.map (fun a'' -> loop a'' b) a')
        in
        loop

    let bind =
        let rec loop a b = match a with
          | Pure a' -> b a'
          | Free a' -> Free (F.map (fun a'' -> loop a'' b) a')
        in
        loop

    let lift f = Free (F.map pure f)

    let iter hnd =
        let rec loop = function
          | Pure a -> a
          | Free f -> hnd (F.map loop f)
        in
        loop

    module IterM(M : Monad) = struct
        let iterM hnd =
            let rec loop = function
              | Pure a -> M.pure a
              | Free f -> hnd (F.map loop f)
            in
            loop
    end
end

module Infix(M : Monad) : sig
    val (>>=) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
end = struct
    let (>>=) = M.bind
end
