(* API for demo programs *)
module Demo = struct
    (* Our functor *)
    module F = struct
        (* This type defines all possible actions *)
        type 'a t = Print of (string * (unit -> 'a))
                  | Random of (int -> 'a)
                  | Abort of string

        (* Need a Functor instance *)
        let map f = function
          | Print (s, next) -> Print (s, fun () -> f (next ()))
          | Random next -> Random (fun rnd -> f (next rnd))
          | Abort s -> Abort s
    end

    (* Magic! *)
    module M = Free.Monad.Make(F)

    include M

    (* Accessors for the actions. These could be generated mechanically. *)
    let print s = lift (F.Print (s, Free.Utils.id))
    let random = lift (F.Random Free.Utils.id)
    let abort s = lift (F.Abort s)
end

(* A demo program *)
let demo_program =
    let open Demo in
    let (>>=) = Demo.bind in

    print "abc" >>= fun () ->
    random >>= fun rnd ->
    if rnd > 42
        then abort "Randoms can't exceed 42!"
        else print (Printf.sprintf "Random was %d" rnd)

(* An interpreter for demo programs. This one uses Pervasives IO *)
let run_demo =
    Demo.iter (function
      | Demo.F.Print (s, n) ->
          Printf.printf "Pervasives says: %s\n" s;
          n ()
      | Demo.F.Random n ->
          n 42
      | Demo.F.Abort s ->
          failwith (Printf.sprintf "Abort: %s" s))

(* An interpreter for demo programs. This uses an underlying monad (Lwt). *)
let run_demo_lwt =
    (* Lwt needs some fiddling for it to conform to our Monad interface *)
    let module DI = Demo.IterM(struct
        include Lwt

        let pure = Lwt.return

        let ap f a =
            let (>>=) = Lwt.bind in
            f >>= fun f' ->
            a >>= fun a' ->
            return (f' a')
    end) in

    DI.iterM (function
      | Demo.F.Print (s, n) ->
          Lwt.bind
              (Lwt_io.printlf "Lwt says: %s" s)
              n
      | Demo.F.Random n ->
          n 43
      | Demo.F.Abort s ->
          Lwt.fail (Failure (Printf.sprintf "Abort: %s" s)))

let main () =
    run_demo demo_program;
    flush_all ();
    Lwt_main.run (run_demo_lwt demo_program)
;;

main ()
