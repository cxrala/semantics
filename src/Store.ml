type loc = string
type t = Store of (loc * int) list

let loc_label n = "l" ^ string_of_int(n)

let create_store n =
  let rec create_store = function
    | 0 -> []
    | x -> (loc_label(n - x), 0) :: create_store (x - 1)
  in Store(create_store n)

let deref store loc =
  let rec deref = function
  | Store([]) -> None
  | Store((l, n)::xs) -> if l == loc then Some(n) 
                          else deref (Store(xs))
  in deref store

let assign (Store(ls)) loc n =
  let rec assign = function
    | [] -> None
    | (l, _)::xs when l == loc -> Some((l, n)::xs)
    | (l, x)::xs -> match assign xs with
                    | None -> None
                    | Some(xss) -> Some((l, x)::xss)
  in match assign ls with
  | None -> None
  | Some(x) -> Some(Store(x))

