open SQueue;;
open Printf;;
open Thread;;

let sq = SQueue.create();;

let tfunc1 () =
  while true do
    let v = SQueue.take sq in
    printf "%s\n" v;
    flush stdout;
    Thread.delay 0.5;
  done

let t1 = Thread.create tfunc1 ();;

while true do
  let l = read_line() in
  SQueue.add l sq;
done
