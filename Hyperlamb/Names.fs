module Names

open Types
open Eval

open System.Collections.Generic

type NamedLambda = 
  { name: string
    lambda: Exp
    encoded: string }

let private nameMap = 
  let m = Dictionary<string, NamedLambda>()
  m.Add("id", { name = "id"; lambda = Lam ("x", Var "x"); encoded = "LR1?x" })
  m

let listAllNamedLambdas() =
  nameMap |> Seq.map (fun e -> e.Key, e.Value) |> Seq.toList

let lookupNamedLambdaByName (name : string) : NamedLambda option = 
  if nameMap.ContainsKey name then Some <| nameMap.Item name else None

let lookupNamedLambdasByExactTokenString (tokenString : string) : NamedLambda list = 
  nameMap.Values |> Seq.filter (fun { name = _; lambda = _; encoded = ts } -> ts = tokenString) |> Seq.toList
  
let lookupNamedLambdasBasicTokenString (tokenString : string) : NamedLambda list = 
  nameMap.Values |> Seq.filter (fun { name = _; lambda = _; encoded = ts } -> ts = tokenString) |> Seq.toList

let nameLambda (name : string) (lambda : Exp) (tokenString : string) =
  nameMap.Add(name, { name = name; lambda = lambda; encoded = tokenString })

let addNamedLambda (namedLambda : NamedLambda) =
  nameMap.Add(namedLambda.name, namedLambda)

let unnameLambda (name : string) = 
  nameMap.Remove name |> ignore

let isNamedLambda (name : string) : bool = 
  nameMap.ContainsKey name

