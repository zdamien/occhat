module SQueue = struct

open Mutex;;
open Condition;;
open Queue;;

type 'a sq_cmds = Add of 'a | Take;;

type 'a t = 'a sq_cmds -> 'a;; 

let create () : 'a t =
  let m = Mutex.create() 
  and c = Condition.create() in
  let q = Queue.create() in
  let clos cmd = match cmd with
  | Add item -> (
    Mutex.lock m;
    Queue.add item q;
    Mutex.unlock m;
    Condition.signal c;
    item
    )
  | Take -> (
    Mutex.lock m;
    while (Queue.is_empty q) do
      Condition.wait c m
    done;
    let item = Queue.take q in
    Mutex.unlock m;
    item
    )
  in clos

let add item q =
  ignore(q (Add item));;

let take q =
  q Take;;

end
