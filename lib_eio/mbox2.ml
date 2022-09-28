type 'a t = {
  senders: unit Waiters.t;
  mutex: Mutex.t;
  id: Ctf.id;
  readers: unit Waiters.t;
  value: 'a option ref;
}

let create () = {
  senders = Waiters.create ();
  mutex = Mutex.create ();
  id = Ctf.mint_id ();
  readers = Waiters.create ();
  value = ref None;
}

let send (t:'a t) (m : string) : unit =
  let sent = ref false in
  let wait () =
    Mutex.lock t.mutex;
    match !(t.value) with
    | Some _ ->
      Waiters.await ~mutex:(Some t.mutex) t.senders t.id
    | None ->
      t.value := Some m;
      Waiters.wake_one' t.readers ();
      sent := true;
      Mutex.unlock t.mutex
  in
  while !sent = false do
    wait ()
  done

let recv t =
  let received = ref None in
  let wait () =
    Mutex.lock t.mutex;
    match !(t.value) with
    | Some v ->
      t.value := None;
      received := Some v;
      Waiters.wake_one' t.senders ();
      Mutex.unlock t.mutex
    | None ->
      Waiters.await ~mutex:(Some t.mutex) t.readers t.id (* XXX enforce just one XXX *)
  in
  while !received = None do
    wait ()
  done;
  Option.get !received
