type 'a t = {
  cond: Condition.t;
  value: 'a option ref;
}

let create () : 'a t =
  { cond = Condition.create ();
    value = ref None; }

let send (t:'a t) (m : 'a) : unit =
  let sent = ref false in
  let wait () =
    Condition.await_interlocked t.cond @@ fun () ->
    match !(t.value) with
    | Some _ -> false
    | None ->
      t.value := Some m;
      sent := true;
      true
  in
  while !sent = false do
    wait ()
  done;
  Condition.broadcast t.cond

let recv t =
  let v = ref None in
  let wait () =
    Condition.await_interlocked t.cond @@ fun () ->
    match !(t.value) with
    | None -> false
    | Some v' -> v := Some v'; t.value := None; true
  in
  while !v = None do
    wait ()
  done;
  Condition.broadcast t.cond;
  Option.get !v
