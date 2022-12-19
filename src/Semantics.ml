open Expression

(* reduce takes one step. *)
let rec reduce (x:Configuration.t):Configuration.t option =
  let (expr, s) = x in
  match expr with
    (*op cases*)
    | Op(Integer(x), op, Integer(y)) -> (match op with
                                        | Plus -> Some((Integer(x + y), s)) (*op +*)
                                        | Geq -> Some((Boolean(x >= y), s))) (*op >=*)
    | Op(Integer(x), op, e2) -> (match reduce (e2, s) with
                                | Some((e, s0)) -> Some((Op(Integer(x), op, e), s0))
                                | _ -> None)
    | Op(e1, op, e2) -> (match reduce (e1, s) with
                        | Some((e, s0)) -> Some((Op(e, op, e2), s0))
                        | _ -> None)
    (*if cases*)
    | If(Boolean(b), e2, e3) -> (match b with
                                | true -> Some((e2, s))
                                | false -> Some((e3, s)))
    | If(e1, e2, e3) -> (match reduce (e1, s) with
                        | Some((e, s0)) -> Some((If(e, e2, e3), s0))
                        | _ -> None)
    (*seq cases*)
    | Seq(Skip, e2) -> Some((e2, s))
    | Seq(e1, e2) -> (match reduce (e1, s) with 
                    | Some((e, s0)) -> Some((Seq(e, e2), s0))
                    | _ -> None)
    (*deref cases*)
    | Deref(l) -> (match Store.deref s l with
                          | Some(x) -> Some((Integer(x), s))
                          | _ -> None)
    (*assign cases*)
    | Assign(l, Integer(x)) -> (match Store.assign s l x with
                                | Some(s0) -> Some((Skip, s0))
                                | _ -> None)
    | Assign(l, e1) -> (match reduce (e1, s) with
                        | Some((e, s0)) -> Some((Assign(l, e), s0))
                        | _ -> None)
    (*while case*)
    | While(e1, e2) -> Some((If(e1, Seq(e2, While(e1, e2)), Skip), s))
    | _ -> None

let rec reduce_to_val (x:Configuration.t) : Configuration.t option =
    match x with
  | (Integer(_), _) -> Some(x)
  | (Boolean(_), _) -> Some(x)
  | (Skip, _) -> Some(x)
  | _ -> (match reduce x with
        | Some(config) -> reduce_to_val config
        | None -> None)