module ChatMod = struct
  open Unix;;
  open Sys;;
  open Printf;;
  open Mutex;;
  open SQueue;;

  type id_type = int;;  (* waffled between keeping as int or string *)

  type message_type =
    | Header of (id_type * int)  (* id and length *)
    | Receipt of id_type
    | Incomplete  (* signals keep trying to get message *)
    | Message of (id_type * bytes)  (* id and message contents *)
    | Bad_header (* signals protocol failure, drop connection *)

type outbound_type =
  | MsgOut of bytes
  | RcpOut of bytes
  | QuitOut

type t = {
  sq: outbound_type SQueue.t; tin:Thread.t; tout:Thread.t; 
  keep_going: bool ref
};;

exception Quit;;

(* inspired by
  https://beej.us/guide/bgnet/output/html/singlepage/bgnet.html#advanced
    *)

let sendall sock msg msg_len =
  let rec sendall_rec pos len =
    let sent = send sock msg pos len [] in
    let total = pos + sent in
    if total = msg_len then ()
    else sendall_rec total (len - sent)
  in sendall_rec 0 msg_len

(* not using
(* These two  from "Unix system programming in OCaml" *)

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let try_finalize f x finally y =
  let res = try f x with exc -> finally y; raise exc in
  finally y;
  res

*)

(* my code *)

exception Connection_closed;;
exception Logic_error;; (* used like assertions, for code paths that
shouldn't happen *)

   
(* message protocol definitions and functions. I wanted to make the
format strings derive from the lengths, to avoid duplicate magic
numbers, but it didn't work well. *)
(* Header layout is HIIIIIILLLLLLLL where H= 1 for messages and 2 for
receipts, I is a text digit representing the message or receipt id, and
L is a text digit represeting message length, ignored by receipts. *)
let msg_id_start = 1;;
let msg_id_len = 6;;
let msg_len_len = 8;;
let msg_len_start = msg_id_start + msg_id_len;;
let header_length = 1 + msg_id_len + msg_len_len;;
let id_to_string id = Printf.sprintf "%06d" id;;
let len_format = "%08d" ^^ "";;
let make_msg_header id len =
  "1" ^ (id_to_string id) ^ Printf.sprintf len_format len;;
let make_receipt id =
  "2"^ id_to_string id ^ "00000000";;

let parse_header buf =
  let first_byte = Bytes.get buf 0 in
  try
    if first_byte = '1' then (* regular message *)
      let msg_id_b = Bytes.sub buf msg_id_start msg_id_len in
      let msg_id_s = Bytes.to_string msg_id_b 
      and msg_len_b = Bytes.sub buf msg_len_start msg_len_len in
      let msg_len_s = Bytes.to_string msg_len_b in
      Header (int_of_string msg_id_s, int_of_string msg_len_s)
    else if first_byte = '2' then
      let msg_id_b = Bytes.sub buf msg_id_start msg_id_len in
      let msg_id_s = Bytes.to_string msg_id_b in
      Receipt (int_of_string msg_id_s)
    else (
      Bad_header
      )
  with 
    | Failure s when s="int_of_string" -> Bad_header
    | x -> raise x

let test() =
(* tests some of the basic helper functions *)
  let x = id_to_string 42 in
  assert (x="000042");
  let x = parse_header (Bytes.of_string "100000100000010") in
  assert (x = Header(1,10));
  let x = parse_header (Bytes.of_string "200000100000000") in
  assert (x = Receipt(1));
  let x = parse_header (Bytes.of_string "3ai000000000000") in
  assert (x = Bad_header);
  let x = make_receipt 1 in
  assert (x="200000100000000");
  let x = make_msg_header 1 10 in
  assert (x="100000100000010");
  ()
  ;;

let readn fd n =
  let buf = Bytes.create n in
  let rec aux rem = try
    let received = recv fd buf (n-rem) rem [] in
    match received with
    | 0 -> raise Quit
    | rem when rem=received -> buf
    | _ -> aux (rem-received)
  with 
  | x -> printf("raising quit\n"); raise Quit
  in aux n
 
let get_message fd =
  let header = readn fd header_length in
  let h = parse_header header in
  match h with
  | Header (id, 0) -> Message (id, (Bytes.create 0))
  | Header (id, len) -> Message (id, readn fd len)
  | Receipt id -> Receipt id
  | x -> x

(* ^ done? ^ --- *)

let create sockfd ui_output ui_error ui_quit =
  let old_sigpipe = signal sigpipe Signal_ignore in
  let keep_going = ref true in
  let id_counter = ref 0 in
  let times_table = Hashtbl.create 123 in
  let tbl_lock = Mutex.create() in
  let sq=SQueue.create() in
  let dummy () = () in
  let tin = ref (Thread.create dummy () )
  and tout = ref (Thread.create dummy () ) in
  let inbound_thr () = ( try
    while !keep_going do
      if Thread.wait_timed_read sockfd 0.1 then
        let msg = get_message sockfd in
        match msg with
        | Message (id, buf) -> (
          ui_output buf;
          let receipt = make_receipt id in
          let receipt_b = Bytes.of_string receipt in
          SQueue.add (RcpOut receipt_b) sq;
          )
        | Receipt id -> (
          try
            let old_time = Hashtbl.find times_table id in
            let diff_time = gettimeofday() -. old_time in (
              ui_error ("msg "^ id_to_string id ^" acked in "
              ^ string_of_float diff_time);
              Mutex.lock tbl_lock;
              ignore(Hashtbl.remove times_table id);
              Mutex.unlock tbl_lock;
            )
          with 
          | Not_found ->
            ui_error "Received receipt for unfound message ID";
        )
        | _ -> (
              ui_error "Badly formed message received, dropping connection";
              keep_going := false;
              SQueue.add QuitOut sq;
              set_signal sigpipe old_sigpipe;
              ui_quit();
              raise Quit;
              )
      else ();
    done;
    printf "quitting inbound\n"; flush Pervasives.stdout;
    with 
    | Quit -> printf "quitting inbound from with\n"; keep_going :=
    false; SQueue.add QuitOut sq; printf "finishing inbound quit\n";
    flush Pervasives.stdout; ui_quit(); Thread.exit();
    | x -> raise x
  ) in
  let outbound_thr () = (try
    while !keep_going do
      let buff = SQueue.take sq in (
        match buff with
        | MsgOut buf ->
          incr id_counter;
          Mutex.lock tbl_lock;
          Hashtbl.replace times_table !id_counter (gettimeofday());
          Mutex.unlock tbl_lock;
          let header = make_msg_header !id_counter (Bytes.length buf) in
          let header_b = Bytes.of_string header in
          let msg_b = Bytes.concat "" [header_b; buf] in
          let msg_b_len = Bytes.length msg_b in
          sendall sockfd msg_b msg_b_len; 
        | RcpOut buf ->
          sendall sockfd buf (Bytes.length buf)
        | QuitOut -> keep_going := false; printf "quitting outbound\n";
        flush Pervasives.stdout; ui_quit(); Thread.exit();
        )
    done
  with x -> raise x
  ) in
  Thread.join !tin;
  Thread.join !tout;
  tin := Thread.create inbound_thr ();
  tout := Thread.create outbound_thr (); 
  {sq=sq; tin= !tin; tout= !tout; keep_going = keep_going}

let send chat buf = (
  SQueue.add (MsgOut buf) chat.sq;
  );;
    
let quit chat =
  printf "in quit chat\n";
  flush Pervasives.stdout;
  SQueue.add QuitOut chat.sq;
  try
    Thread.join chat.tin;
    Thread.join chat.tout;
    printf "leaving quit chat\n";
    flush Pervasives.stdout
  with x -> (printf "weird exception\n"; flush Pervasives.stdout);;

let time_to_quit chat =
  not !(chat.keep_going);;

let join chat =
  try
    Thread.join chat.tout;
    Thread.join chat.tin;
  with x -> printf "weird exception\n"; flush Pervasives.stdout;

end

