module Something

open Types
open Names

let rec listBoundVariables exp =
  match exp with
  | Var v -> []
  | Lam (v, e) ->
    v :: listBoundVariables e
  | App (e1, e2) ->
    listBoundVariables e1 @ listBoundVariables e2

let toTokenString tokens = 
  let t2s t = 
    match t with 
    | Reference n -> sprintf "R%d" n
    | Abstraction -> "L"
    | Application -> "A"
  let strs = tokens |> List.map t2s
  String.concat "" strs

let rec tokenify scope exp : Token list = 
  match exp with
  | Var v -> 
    let index = scope |> List.findIndex (fun it -> it = v)
    [ Reference (index + 1) ]
  | Lam (v, e) ->
    let tokens = tokenify (v :: scope) e 
    Abstraction :: tokens
  | App (e1, e2) ->
    let tokens1 = tokenify scope e1
    let tokens2 = tokenify scope e2
    Application :: (tokens1 @ tokens2)

let toQueryString (bounds : string list) = 
  if bounds.Length = 0 then ""
  else bounds |> String.concat "&" |> sprintf "?%s"

let rec replaceNames (names : NamedLambda list) = function
  | Name n ->
    let maybe = names |> List.tryFind (fun nl -> nl.name = n)
    match maybe with
    | Some nl -> nl.lambda
    | None -> failwith <| sprintf "Lambda refers to unknown name %s" n
  | VarN v -> Var v
  | LamN (p, e) -> Lam (p, replaceNames names e)
  | AppN (f, x) -> App (replaceNames names f, replaceNames names x)      