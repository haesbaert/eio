open Eio.Std

let mbox = Eio.Mbox.create ()

let sleepy_time clock =
  Eio.Time.sleep clock @@
  Random.float 1.0

let sender ~clock s =
  while true do
    Eio.Mbox.send mbox s;
    sleepy_time clock
  done

let receiver ~clock =
  while true do
    let m = Eio.Mbox.recv mbox in
    traceln "m=%s" m;
    sleepy_time clock
  done

let main ~domain_mgr ~clock =
  let domain_fork ~sw (f: unit -> unit) =
    Fiber.fork ~sw (fun () ->
        Eio.Domain_manager.run domain_mgr f)
  in
  Switch.run @@ fun sw ->
  domain_fork ~sw (fun () -> sender "That's just like, your opinion man" ~clock);
  domain_fork ~sw (fun () -> sender "Shut up donnie" ~clock);
  domain_fork ~sw (fun () -> sender "That rug really tied the room together" ~clock);
  domain_fork ~sw (fun () -> sender "You've no frame of reference" ~clock);
  domain_fork ~sw (fun () -> sender "The dude abides" ~clock);
  domain_fork ~sw (fun () -> sender "Fucking Quintanna man, that creep can roll" ~clock);
  domain_fork ~sw (fun () -> receiver ~clock)

let () =
  Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env) ~clock:(Eio.Stdenv.clock env)
