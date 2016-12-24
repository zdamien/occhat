module ChatMod = struct
  open Unix;;
  open Sys;;
  open Printf;;


  type id_type = int;;  (* waffled between keeping as int or string *)

  type message_type =
    | Header of (id_type * int)  (* id and length *)
    | Receipt of id_type
    | Incomplete  (* signals keep trying to get message *)
    | Message of (id_type * bytes)  (* id and message contents *)
    | Bad_header (* signals protocol failure, drop connection *)

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

(* These two  from "Unix system programming in OCaml"... never got
around to using them. *)

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> restart_on_EINTR f x

let try_finalize f x finally y =
  let res = try f x with exc -> finally y; raise exc in
  finally y;
  res

(* my code *)

exception Connection_closed;;
exception Logic_error;; (* used like assertions, for code paths that
shouldn't happen *)

(* buffered socket reader. takes socket and expected message size,
returns a function : unit -> bytes option, which can be called when the
socket is ready until the buffer is full.  Returned function returns
Some buf when its buffer is full.  Currently copies the buffer for
return, so callers can use it with impunity.  *)
let make_buffer_reader sock msgsize =
  let total_received = ref 0 in
  let remaining () = msgsize - !total_received in
  let buf = Bytes.create msgsize in
  let get_msg () =
    try
      let received_now = recv sock buf !total_received (remaining()) [] in
      match received_now with
      | 0 -> raise Connection_closed
      | _ ->  (
        total_received := !total_received + received_now;
        if !total_received = msgsize then (
          total_received := 0;
          Some (Bytes.copy buf)
        )
        else
          None
      )
    with x -> raise x;
  in
  get_msg;;

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

(*
type id_type = int;;  (* waffled between keeping as int or string *)

type message_type = 
  | Header of (id_type * int)  (* id and length *)
  | Receipt of id_type
  | Incomplete  (* signals keep trying to get message *)
  | Message of (id_type * bytes)  (* id and message contents *)
  | Bad_header (* signals protocol failure, drop connection *)
  *)

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

(* like make_buffer_reader, but for entire messages *)
let make_message_reader sock =
  let no_header = ref true in
  let msg_id = ref (-1) in
  let msg_reader = ref (make_buffer_reader sock 0) in (* dummy default *)
  let header_reader = make_buffer_reader sock header_length in
  let get_message () =
    if !no_header then
      let buf = header_reader () in
      match buf with
      | Some buf -> (
        let header = parse_header buf in
        match header with
        | Header (id, len) -> 
          if len > 0 then (
            no_header := false;
            msg_id := id;
            msg_reader := make_buffer_reader sock len;
            Incomplete
          ) else
          Message (id, "")
        | Message _ -> raise Logic_error (* shoudn't go from having no header
        to a complete Message *)
        | x -> x (* Incomplete, Receipt, and Bad_header cases *)
      )
      | None -> Incomplete (* we're still trying to read a complete
      header! *)
    else  (* we have already parsed a header and set msg_reader *)
      let buf = (!msg_reader) () in
      match buf with
      | None -> Incomplete
      | Some buf -> 
        no_header := true; 
        Message (!msg_id, buf);
  in get_message;;

let get_input () =
  let line = read_line () in
  if line = "/quit" then (true, "") else (false, Bytes.of_string line)

let output_func msg =
  match msg with
  | Message (_, buf) -> print_endline (Bytes.to_string buf)
  | _ -> raise Logic_error

(* the workhorse function.  Needs to be passed three callback functions:
ui_input : unit -> (quit: bool * chat_msg: bytes)
returns (true, _) if the user has entered a quit command, otherwise
(false_ chat_msg) where chat_msg is output ready to go on the network.
ui_output : Message ('a, bytes) -> (), unwraps a chat message and displays
the contents
ui_error : string -> (), displays error or status messages for the user.

caller is responsible for closing sockets passed in.
*)
let chat chat_sock ui_fd ui_input ui_output ui_error = 
  let old_sigpipe = signal sigpipe Signal_ignore in
  let listening = [chat_sock; ui_fd] in
  let keep_going = ref true in
  let id_counter = ref 0 in
  let times_table = Hashtbl.create 123 in
  let get_message = make_message_reader chat_sock in
  let do_chat sock =
    if sock == chat_sock then
      try
        let msg = get_message() in
        match msg with
        | Incomplete -> ()
        | Message (id, buf) -> (
          ui_output msg; 
          let receipt = make_receipt id in
          let receipt_b = Bytes.of_string receipt in
          sendall chat_sock receipt_b (Bytes.length receipt_b);
          ()
        )
        | Receipt id -> (
          try
            let old_time = Hashtbl.find times_table id in
            let diff_time = gettimeofday() -. old_time in (
              ui_error ("msg "^ id_to_string id ^" acked in "
              ^ string_of_float diff_time);
              Hashtbl.remove times_table id
              )
          with 
          | Not_found -> ui_error "Received receipt for unfound message ID"
          (* could use this as a reason to drop the connection, but am
          choosing otherwise *)
        )
        | Bad_header -> 
            ui_error "Badly formed message received, dropping connection";
            keep_going := false
        | Header _ -> raise Logic_error

      with
      | Connection_closed ->
        keep_going := false;
        ui_error "Connection closed remotely"
        
    else  (* socket is UI socket *)
      let quit, msg = ui_input() in
      if quit then (
        keep_going := false;
        shutdown chat_sock SHUTDOWN_SEND
      )
      else (
        incr id_counter;
        Hashtbl.replace times_table !id_counter (gettimeofday());
        let header = make_msg_header !id_counter (Bytes.length msg) in
        let header_b = Bytes.of_string header in
        let msg_b = Bytes.concat "" [header_b; msg] in
        let msg_b_len = Bytes.length msg_b in
        sendall chat_sock msg_b msg_b_len; 
        ()
      )
    in
    while !keep_going do
      let rl, _, _ = select listening [] [] (-1.) in
      List.iter do_chat rl
    done;
    set_signal sigpipe old_sigpipe;

end

