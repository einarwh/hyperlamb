module Eval

open Types
open System

type EvalResult =
    | Next of Exp
    | Normal

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

let rec allIds = function
    | Var v -> Set.singleton v
    | Lam (p, x) -> Set.add p (allIds x)
    | App (f, a) -> Set.union (allIds f) (allIds a)

let freeIds x =
    let rec halp bound = function
        | Var v -> if Set.contains v bound then Set.empty else Set.singleton v
        | Lam (p, x) -> halp (Set.add p bound) x
        | App (f, a) -> Set.union (halp bound f) (halp bound a)
    halp Set.empty x

type ConflictResult =
    | Fine
    | Renamed of Exp

let idNum (s : string) =
    let rec halp i =
        if i = -1 || not (Char.IsDigit s.[i])
        then i
        else halp (i - 1)
    let stop = s.Length - 1
    let res = halp stop
    if res = stop
    then s, 1
    else s.[0 .. res], int(s.[res + 1 .. stop])

let uniqueId taken s =
    let prefix, start = idNum s
    let rec halp i =
        let newId = prefix + string(i)
        if Set.contains newId taken
        then halp (i + 1)
        else newId
    halp (start + 1)

let rename all (t, s, x) =
    let free = freeIds t
    let rec halp = function
        | Var v -> Fine
        | App (f, a) ->
            match halp f with
                | Renamed rf -> Renamed (App (rf, a))
                | _ ->
                    match halp a with
                        | Renamed ra -> Renamed (App (f, ra))
                        | _ -> Fine
        | Lam (p, b) ->
            if (p = s) || (not (Set.contains s (freeIds b))) || (not (Set.contains p free))
            then Fine
            else
                let newP = uniqueId all p
                Renamed (Lam (newP, subst (Var newP, p, b)))

    match halp x with
        | Renamed x -> Renamed (App (Lam (s, x), t))
        | _ -> Fine

let reduce x =
    let all = allIds x
    let rec halp = function
        | Var v -> Normal
        | App (Lam (p, b), a) ->
            let redex = a, p, b
            match rename all redex with
                | Renamed x -> Next x
                | Fine -> Next (subst redex)
        | App (f, a) ->
            match halp f with
                | Next rf -> Next (App (rf, a))
                | _ ->
                    match halp a with
                        | Next ra -> Next (App (f, ra))
                        | _ -> Normal
        | Lam (p, b) ->
            match halp b with
                | Next b -> Next (Lam (p, b))
                | _ -> Normal
    halp x
