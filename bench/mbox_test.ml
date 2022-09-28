open Eio.Std

let mbox = Eio.Mbox.create ()

let total_sent = Atomic.make 0

let sleepy_time clock =
  Eio.Time.sleep clock @@
  Random.float 0.5

let sender ~clock s =
  while true do
    Eio.Mbox.send mbox s;
    Atomic.incr total_sent;
    sleepy_time clock
  done

let receiver ~clock =
  let rec loop = function
    | 100_000 ->
      Eio.Time.sleep clock 5.0; (* XXX RACY *)
      traceln "received 100_000 messages, total_sent is %d" (Atomic.get total_sent)
    | n ->
      let m = Eio.Mbox.recv mbox in
      traceln "%d\t%s" n m;
      sleepy_time clock;
      loop (succ n)
  in
  loop 0

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
