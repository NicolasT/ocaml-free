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
    val (<$>) : ('a -> 'b) -> 'a M.t -> 'b M.t
    val (<*>) : ('a -> 'b) M.t -> 'a M.t -> 'b M.t
    val (>>=) : 'a M.t -> ('a -> 'b M.t) -> 'b M.t
end = struct
    module A = Free_applicative.Infix(M)
    include A

    let (>>=) = M.bind
end

module Utils(M : Monad) : sig
    module Infix : sig
        val (<$>) : ('a -> 'b) -> 'a M.t -> 'b M.t
        val (<$) : 'a -> 'b M.t -> 'a M.t

        val (<*>) : ('a -> 'b) M.t -> 'a M.t -> 'b M.t
        val (<**>) : 'a M.t -> ('a -> 'b) M.t -> 'b M.t

        val ( *>) : 'a M.t -> 'b M.t -> 'b M.t
        val (<*) : 'a M.t -> 'b M.t -> 'a M.t

        val (<=<) : ('b -> 'c M.t) -> ('a -> 'b M.t) -> 'a -> 'c M.t
        val (=<<) : ('a -> 'b M.t) -> 'a M.t -> 'b M.t
        val (>=>) : ('a -> 'b M.t) -> ('b -> 'c M.t) -> 'a -> 'c M.t
    end

    val map : ('a -> 'b) -> 'a M.t -> 'b M.t

    val pure : 'a -> 'a M.t
    val ap : ('a -> 'b) M.t -> 'a M.t -> 'b M.t

    val liftA : ('a -> 'b) -> 'a M.t -> 'b M.t
    val liftA2 : ('a -> 'b -> 'c) -> 'a M.t -> 'b M.t -> 'c M.t
    val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a M.t -> 'b M.t -> 'c M.t -> 'd M.t

    val bind : 'a M.t -> ('a -> 'b M.t) -> 'b M.t

    val filterM : ('a -> bool M.t) -> 'a list -> 'a list M.t
    val foldM : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> 'a M.t
    val foldM_ : ('a -> 'b -> 'a M.t) -> 'a -> 'b list -> unit M.t
    val forM : 'a list -> ('a -> 'b M.t) -> 'b list M.t
    val forM_ : 'a list -> ('a -> 'b M.t) -> unit M.t
    val forever : 'a M.t -> 'b M.t
    val join : 'a M.t M.t -> 'a M.t
    val liftM : ('a -> 'b) -> 'a M.t -> 'b M.t
    val liftM2 : ('a -> 'b -> 'c) -> 'a M.t -> 'b M.t -> 'c M.t
    val liftM3 : ('a -> 'b -> 'c -> 'd) -> 'a M.t -> 'b M.t -> 'c M.t -> 'd M.t
    val liftM4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a M.t -> 'b M.t -> 'c M.t -> 'd M.t -> 'e M.t
    val liftM5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a M.t -> 'b M.t -> 'c M.t -> 'd M.t -> 'e M.t -> 'f M.t
    val mapAndUnzipM : ('a -> ('b * 'c) M.t) -> 'a list -> ('b list * 'c list) M.t
    val mapM : ('a -> 'b M.t) -> 'a list -> 'b list M.t
    val mapM_ : ('a -> 'b M.t) -> 'a list -> unit M.t
    val replicateM : int -> 'a M.t -> 'a list M.t
    val replicateM_ : int -> 'a M.t -> unit M.t
    val sequence : 'a M.t list -> 'a list M.t
    val sequence_ : 'a M.t list -> unit M.t
    val unless : bool -> unit M.t -> unit M.t
    (* `void` could be defined on Functor, but seems rather useless *)
    val void : 'a M.t -> unit M.t
    val when_ : bool -> unit M.t -> unit M.t
    val zipWithM : ('a -> 'b -> 'c M.t) -> 'a list -> 'b list -> 'c list M.t
    val zipWithM_ : ('a -> 'b -> 'c M.t) -> 'a list -> 'b list -> unit M.t
end = struct
    include M

    module A = Free_applicative.Utils(M)

    module Infix = struct
        include A.Infix
        module MI = Infix(M)
        include MI

        let (>=>) a b = fun x -> a x >>= b
        let (<=<) a b = b >=> a
        let (=<<) a b = b >>= a
    end

    include (A : sig
        val liftA : ('a -> 'b) -> 'a M.t -> 'b M.t
        val liftA2 : ('a -> 'b -> 'c) -> 'a M.t -> 'b M.t -> 'c M.t
        val liftA3 : ('a -> 'b -> 'c -> 'd) -> 'a M.t -> 'b M.t -> 'c M.t -> 'd M.t
    end)

    open Infix

    let filterM p =
        let rec loop = function
          | [] -> pure []
          | (x :: xs) ->
                p x >>= fun k ->
                loop xs >>= fun r ->
                pure (if k then x :: r else r)
        in
        loop

    let foldM f =
        let rec loop a = function
          | [] -> pure a
          | (x :: xs) ->
                f a x >>= fun a' ->
                loop a' xs
        in
        loop

    let foldM_ f a l = foldM f a l >>= fun _ -> pure ()

    let sequence ls =
        let helper t0 t1 =
            t0 >>= fun x ->
            t1 >>= fun xs ->
            pure (x :: xs)
        in
        List.fold_right helper ls (pure [])

    let sequence_ ls = List.fold_right (fun a b -> a >>= fun _ -> b) ls (pure ())

    let mapM f l = sequence (List.map f l)
    let mapM_ f l = sequence_ (List.map f l)

    let forM l f = mapM f l
    let forM_ l f = mapM_ f l

    let forever =
        let rec loop f = f >>= fun _ -> loop f in
        loop

    let join a = a >>= Free_utils.id

    let liftM = liftA
    let liftM2 = liftA2
    let liftM3 = liftA3
    let liftM4 f a b c d = liftM3 f a b c <*> d
    let liftM5 f a b c d e = liftM4 f a b c d <*> e

    let void f = map (fun _ -> ()) f
    let when_ b f = if b then f else pure ()
    let unless b f = if b then pure () else f

    let replicateM c f = sequence (Free_utils.replicate c f)
    let replicateM_ c f = sequence_ (Free_utils.replicate c f)

    let zipWithM f xs ys = sequence (Free_utils.zipWith f xs ys)
    let zipWithM_ f xs ys = sequence_ (Free_utils.zipWith f xs ys)

    let mapAndUnzipM f xs = sequence (List.map f xs) >>= fun r -> pure (List.split r)
end
