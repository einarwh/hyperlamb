module Names

open System.Collections.Generic

open SQLite

open Types
open Eval

type NamedLambda = 
  { name: string
    lambda: Exp
    encoded: string }

type NamedLambdaEntry() =
    let mutable name : string = null
    let mutable encoded : string = null
    member this.Name 
        with get() = name 
        and set(value) = name <- value
    member this.Encoded 
        with get() = encoded 
        and set(value) = encoded <- value

let db = new SQLiteConnection("lambdb"); 
db.CreateTable<NamedLambdaEntry>() |> ignore
printfn "DatabasePath %s" db.DatabasePath
let info = db.GetTableInfo("NamedLambdaEntry")
info |> Seq.iter (fun col -> printfn "%s" col.Name)
let mutable persistedNameCount = 0
let maxPersistedNameCount = 50

let private nameMap = 
  let m = Dictionary<string, NamedLambda>()
  let entries = db.Query<NamedLambdaEntry>("SELECT * FROM NamedLambdaEntry")
  printfn "Number of frozen names %d" <| entries.Count
  persistedNameCount <- entries.Count 
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

let addNamedLambda (namedLambda : NamedLambda) =
  nameMap.Add(namedLambda.name, namedLambda)
  if persistedNameCount < maxPersistedNameCount then
    let entry = NamedLambdaEntry() 
    entry.Name <- namedLambda.name
    entry.Encoded <- namedLambda.encoded
    db.Insert(entry) |> ignore
    persistedNameCount <- persistedNameCount + 1
  else
    printfn "Too many names, I don't want too many names."

let nameLambda (name : string) (lambda : Exp) (tokenString : string) =
  let nl = { name = name; lambda = lambda; encoded = tokenString }
  addNamedLambda nl

let unnameLambda (name : string) = 
  nameMap.Remove name |> ignore

let isNamedLambda (name : string) : bool = 
  nameMap.ContainsKey name

