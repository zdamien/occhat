module SQueue = struct

open Mutex;;
open Condition;;
open Queue;;

type 'a sq_cmds = Add of 'a | Take;;

type 'a t = { m: Mutex.t; c: Condition.t; q: 'a Queue.t };;

let create () =
  { m=Mutex.create() ; c=Condition.create(); q=Queue.create() };;

let add item sq =
  Mutex.lock sq.m;
  Queue.add item sq.q;
  Condition.signal sq.c;
  Mutex.unlock sq.m;;

let take sq =
  Mutex.lock sq.m;
  while (Queue.is_empty sq.q) do
    Condition.wait sq.c sq.m
  done;
  let item = Queue.take sq.q in
  Mutex.unlock sq.m;
  item;;

let length sq =
  Mutex.lock sq.m;
  let v = Queue.length sq.q in
  Mutex.unlock sq.m;
  v;;

end
