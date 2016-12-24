module ChatMod : sig

  exception Connection_closed;;
  exception Logic_error;; (* used like assertions, for code paths that
  shouldn't happen *)

  type t

(* First argument is chat socket.  Also eeds to be passed three callback
functions:
ui_output : bytes -> unit unwraps a chat message and
displays
the contents
ui_error : string -> unit displays error or status messages for the user.
ui_quit : unit -> unit  , tells the creator to quit

returns a chat instance.

*)

  val create : (Unix.file_descr) -> (bytes -> unit) -> (string->unit) -> (unit->unit) -> t

(* send a bytes blob as message. Queues message internally *)
  val send : t -> bytes -> unit

  val test : unit -> unit

  val quit : t -> unit
  val join : t -> unit

  val time_to_quit : t -> bool
end
