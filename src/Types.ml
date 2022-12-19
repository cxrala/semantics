open Expression
type t =
  | Int
  | Bool
  | Unit
  | Intref

type type_env = (Store.loc * t) list

let rec get_t (store:type_env) l =
  match store with
  | [] -> None
  | ((x:Store.loc), type_ref)::xs -> if x == l then Some(type_ref)
                                     else get_t xs l

let get_type = function
  | Geq -> Bool
  | Plus -> Int

let rec infer (gamma:type_env) = function
  | Integer(_) -> Some(Int)
  | Boolean(_) -> Some(Bool)
  | Skip -> Some(Unit)
  | Op(e1, op, e2)-> (match (infer gamma e1, infer gamma e2) with
                      | Some(Int), Some(Int) -> Some(get_type op)
                      | _ -> None)
  | If(e1, e2, e3) -> (match (infer gamma e1, infer gamma e2, infer gamma e3) with
                      | Some(Bool), Some(x), Some(y) when x = y -> Some(x)
                      | _ -> None)
  | Seq(e1, e2) -> (match (infer gamma e1, infer gamma e2) with
                    | Some(Unit), Some(x) -> Some(x)
                    |_ -> None)
  | Deref(l) -> get_t gamma l
  | Assign(l, n) -> (match (get_t gamma l, infer gamma n) with
                    | Some(Intref), Some(Int) -> Some(Unit)
                    | _ -> None)
  | While(e1, e2) -> (match (infer gamma e1, infer gamma e2) with
                    | Some(Bool), Some(Unit) -> Some(Unit)
                    | _ -> None)