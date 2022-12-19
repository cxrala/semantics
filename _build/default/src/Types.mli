type t =
    | Int
    | Bool
    | Unit
    | Intref

type type_env = (Store.loc * t) list

val infer : type_env -> Expression.t -> t option