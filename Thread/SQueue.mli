module SQueue : sig

  type 'a t
  val create : unit -> 'a t
  val add : 'a -> 'a t -> unit
  val take : 'a t -> 'a
  val length : 'a t -> int

end

