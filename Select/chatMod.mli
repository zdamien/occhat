module ChatMod : sig

  exception Connection_closed;;
  exception Logic_error;; (* used like assertions, for code paths that
  shouldn't happen *)

  type id_type = int;;  (* waffled between keeping as int or string *)

  type message_type =
    | Header of (id_type * int)  (* id and length *)
    | Receipt of id_type
    | Incomplete  (* signals keep trying to get message *)
    | Message of (id_type * bytes)  (* id and message contents *)
    | Bad_header (* signals protocol failure, drop connection *)

  val chat : (Unix.file_descr) -> (Unix.file_descr) ->  (unit -> (bool * bytes)) ->
  (message_type -> unit) -> (string->unit) -> unit

  val test : unit -> unit
end
