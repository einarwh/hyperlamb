module Eval

open Types

let rec subst = function
    | t, s, App (f, a) -> App (subst (t, s, f), subst (t, s, a))
    | t, s, Lam (p, b) ->
        if p = s
        then Lam (p, b)
        else Lam (p, subst (t, s, b))
    | t, s, Var v ->
        if v = s
        then t
        else Var v

let rec reduce = function
    | App (Lam (p, b), a) -> subst (a, p, b)
    | App (f, a) -> App (reduce f, reduce a)
    | Lam (p, b) -> Lam (p, reduce b)
    | Var v -> Var v

let rec reduceAll t =
    let res = reduce t
    if res = t
    then res
    else reduceAll res