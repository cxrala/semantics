type t
type loc = string
val create_store : int -> t
val deref : t -> loc -> int option
val assign : t -> loc -> int -> t option