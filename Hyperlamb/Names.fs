module Names

open System
open System.Collections.Generic

open SQLite

open Types
open Eval
open TokenParser
open ParseExpThings

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

let isAcceptableName (name : string) = 
  let maxNameLength = 50
  name.Length <= maxNameLength && name |> Seq.forall Char.IsLetterOrDigit

let db = new SQLiteConnection("lambdb"); 
db.CreateTable<NamedLambdaEntry>() |> ignore
printfn "DatabasePath %s" db.DatabasePath
let info = db.GetTableInfo("NamedLambdaEntry")
info |> Seq.iter (fun col -> printfn "%s" col.Name)
let mutable persistedNameCount = 0
let maxPersistedNameCount = 50

let private parseQueryString qs = 
  let tmp = String.split '&' qs
  tmp |> List.map (fun s -> (s, None))

let private toNamedLambda (nle : NamedLambdaEntry) : NamedLambda option = 
  let enc = nle.Encoded
  let ix = enc.IndexOf('?')
  let (lamb, vars) = 
    if ix < 0 then (enc, []) 
    else 
      let qps = parseQueryString <| enc.Substring(ix)
      (enc.Substring(0, ix), getVars qps)
  let maybeExp = parseExp tokenParser lamb vars
  let result = maybeExp |> Option.map (fun e -> { name = nle.Name; lambda = e; encoded = nle.Encoded })
  result
  
let private nameMap = 
  let m = Dictionary<string, NamedLambda>()
  m.Add("id", { name = "id"; lambda = Lam ("x", Var "x"); encoded = "LR1?x" })
  let entries = db.Query<NamedLambdaEntry>("SELECT * FROM NamedLambdaEntry")
  printfn "Number of frozen names %d" <| entries.Count
  let namedLambdas = entries |> Seq.map toNamedLambda |> Seq.choose id |> Seq.toList 
  namedLambdas |> Seq.iter (fun nl -> printfn " - %s" nl.name) 
  namedLambdas |> Seq.iter (fun nl -> m.Add(nl.name, nl))
  persistedNameCount <- entries.Count 
  m

let listAllNamedLambdas() =
  nameMap |> Seq.map (fun e -> e.Key, e.Value) |> Seq.toList

let lookupNamedLambdaByName (name : string) : NamedLambda option = 
  if nameMap.ContainsKey name then Some <| nameMap.Item name else None

let lookupNamedLambdaByPartialName (name : string) : NamedLambda list = 
  let namedLambdas = nameMap.Values |> Seq.toList
  let matches = namedLambdas |> List.filter (fun v -> v.name.StartsWith(name))
  matches

let lookupNamedLambdaByUglyName () : NamedLambda list = 
  let namedLambdas = nameMap.Values |> Seq.toList
  let matches = namedLambdas |> List.filter (fun nl -> not <| isAcceptableName nl.name)
  matches

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
  let q = sprintf "DELETE FROM NamedLambdaEntry WHERE Name = \"%s\"" name
  db.Execute(q) |> ignore

let isNamedLambda (name : string) : bool = 
  nameMap.ContainsKey name

