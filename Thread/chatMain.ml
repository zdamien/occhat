(** Simple dual mode chat program.

program server <port number>
program client <IP address> <port number>

Server mode accepts only a single connection at a time.

UI/wire isolation: chat() uses select() on the socket and a UI file
descriptor parameter, and invokes UI function arguments when the UI
filedes is ready.  It doesn't assume that UI input is actually coming
from a file.  Likewise, chat() calls a UI output function to display
received messages.  Even stronger separation could be achieved with UI
and wire threads communicating via synchronized queues.

Message IDs are a counter incremented by the sender; if you switch
between two programs talking to each other, you may see two "message
1"s, but that's not a bug, that's just each end having its own idea of
what message 1 is.

*)

open Unix;;
open Sys;;
open Printf;;
open ChatMod;;
open Thread;;

let get_input () =
  let line = read_line () in
  if line = "/quit" then (true, "") else (false, Bytes.of_string line)

let output_func msg =
  print_endline (Bytes.to_string msg); flush Pervasives.stdout;
(*
  match msg with
  | ChatMod.Message (_, buf) -> print_endline (Bytes.to_string buf)
  | _ -> raise (Failure "bad message used in output_func")
  *)


exception Quit;;

let to_quit = ref false;;
let quit_func () = to_quit := true;;

let client server port =
  print_endline ("client "^server^" "^string_of_int(port));
  let server_ip = (gethostbyname server).h_addr_list.(0) in
  let server_port = ADDR_INET (server_ip, port) in
  let client_socket = socket PF_INET SOCK_STREAM 0 in
  try
    connect client_socket server_port;
    let chat = ChatMod.create client_socket output_func print_endline
        quit_func in
      while true do try
          let can_read = Thread.wait_timed_read Unix.stdin 0.1 in
          if can_read then (
            let line = read_line() in
            let buf = Bytes.of_string line in
            ChatMod.send chat buf; );
          if ChatMod.time_to_quit chat then (raise Quit;);
        with x -> ChatMod.quit chat; raise Quit
      done;
    close client_socket;
  with 
  | Quit -> printf("Connection closed, quitting\n"); close client_socket; 
  | x -> printf("Terminal exception, quitting\n"); close client_socket; raise x;;

let server port =
  print_endline ("server "^string_of_int(port));
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  try
    let server_ip = inet_addr_any in
    let server_port = ADDR_INET (server_ip, port) in
    bind server_socket server_port;
    listen server_socket 1;
    while true do
      let chat_sock, _ = accept server_socket in
      try 
        print_endline "accepting connection";
        let chat = ChatMod.create chat_sock output_func print_endline
          quit_func in
        while true do try
          let can_read = Thread.wait_timed_read Unix.stdin 0.1 in
          if can_read then (
            let line = read_line() in
            let buf = Bytes.of_string line in
            ChatMod.send chat buf; );
          if ChatMod.time_to_quit chat then raise Quit;
          with x -> ChatMod.quit chat; raise Quit
        done;
        close chat_sock;
      with 
      | Quit -> printf("Connection closed, quitting\n"); close chat_sock; 
      | x -> printf("Terminal exception, quitting\n"); close chat_sock; raise x;
    done;
    close server_socket  (* not that we should ever get here *)
  with x -> close server_socket; raise x

let help = "Usage:\n\
occhat server <port number>\n\
occhat client <host name or IP> <port number>\
"

let main () =
  try
    match Sys.argv with
    | [| _; "client"; mserver; port |] -> client mserver (int_of_string port)
    | [| _; "server"; port |] -> server (int_of_string port)
    | _ -> printf "Invalid arguments.\n%s\n" help
  with
  | Invalid_argument s -> printf "2 %s %s" s help
  | Failure s when s="int_of_string" -> printf "Non-numeric port given.\n%s\n" help
  | Failure s -> printf "Failure %s.\n%s\n" s help
  | Not_found -> printf "Host not found.\n" 
  | Unix.Unix_error (ECONNREFUSED, "connect", "") -> printf
  "Connection refused.\n"
  | Unix.Unix_error (EADDRINUSE, "bind", "") -> printf "Port not bindable\n"
  | x -> printf "Error\n"; raise x
  ;;

main();

