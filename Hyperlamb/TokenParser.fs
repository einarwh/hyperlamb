module TokenParser

open FParsec

open Types

type VarType = NamedLambdaVar | OrdinaryVar 

let getVars (qps : (string * string option) list) : (string * VarType) list = 
  let defaults = ['a' .. 'z'] |> List.map (fun c -> string(c))
  if qps.Length = 0 then
    defaults |> List.map (fun n -> n, OrdinaryVar)
  else
    let qvars = qps |> List.map (fun (k, v) -> k, match v with Some "name" -> NamedLambdaVar | _ -> OrdinaryVar) 
    let qvarnames = qvars |> List.map (fun (n, t) -> n)
    let additional = defaults |> List.except qvarnames
    let vars = qvars @ (additional |> List.map (fun n -> n, OrdinaryVar))
    vars

let tokenParser, tokenParserRef = createParserForwardedToRef<Token list, unit>()

let abstractionTokenParser = (pchar 'L') |>> (fun _ -> Abstraction)
let applicationTokenParser = (pchar 'A') |>> (fun _ -> Application)

let referenceTokenParser = 
  pipe2 (pchar 'R') 
    (many1 digit) 
    (fun _ cs -> 
      System.String(cs |> Array.ofList) |> int |> Reference)

let singleTokenParser =
  abstractionTokenParser <|> applicationTokenParser <|> referenceTokenParser

do tokenParserRef := many1 singleTokenParser
