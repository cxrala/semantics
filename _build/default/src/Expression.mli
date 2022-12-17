type t =
  | Integer of int
  | Boolean of bool
  | Skip
  | While of t * t
  | If of t * t * t
  | Seq of t * t
  | Deref of Store.loc
  | Assign of Store.loc * t
  | Op of t * t