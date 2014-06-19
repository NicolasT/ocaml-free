(* API for demo programs *)
module Demo = struct
    (* Our functor *)
    module F = struct
        (* This type defines all possible actions *)
        type 'a t = Print of (string * (unit -> 'a))
                  | Random of (int -> 'a)

        (* Need a Functor instance *)
        let map f = function
          | Print (s, next) -> Print (s, fun () -> f (next ()))
          | Random next -> Random (fun rnd -> f (next rnd))
    end

    (* Magic! *)
    module M = Free.Monad.Make(F)

    include M

    (* Accessors for the actions. These could be generated mechanically. *)
    let print s = lift (F.Print (s, Free.Utils.id))
    let random = lift (F.Random Free.Utils.id)
end

(* A demo program *)
let demo_program =
    let open Demo in
    let (>>=) = Demo.bind in

    print "abc" >>= fun () ->
    random >>= fun rnd ->
    print (Printf.sprintf "Random was %d" rnd)

(* An executor for a demo programs. This one uses Pervasives IO *)
let run_demo =
    Demo.iter (function
      | Demo.F.Print (s, n) ->
          Printf.printf "Pervasives says: %s\n" s;
          n ()
      | Demo.F.Random n ->
          n 42)

(* An executor for a demo program. This uses an underlying monad (Lwt). *)
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
          n 43)

let main () =
    run_demo demo_program;
    flush_all ();
    Lwt_main.run (run_demo_lwt demo_program)
;;

main ()
