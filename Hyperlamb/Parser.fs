module Parser 

open System
open Types
open FParsec

let expParser, expParserRef = createParserForwardedToRef<Exp, unit>()

let varNameParser = 
  many1 lower |>> (fun cs -> new String(List.toArray(cs)))

let varParser = varNameParser |>> Var
let lamParser = 
        pipe2 ((pchar 'λ' <|> pchar '\\') >>. varNameParser) 
              (pchar '.' >>. expParser)
              (fun s e -> Lam (s, e))
let parParser = between (pchar '(') (pchar ')') expParser
let notAppParser = lamParser <|> varParser <|> parParser

let appParser = 
  let applify defs =
    let leftest = List.head defs
    let restest = List.tail defs
    List.fold (fun acc e -> App (acc, e)) leftest restest 
  sepBy1 notAppParser (pchar ' ') |>> applify

do expParserRef := attempt appParser <|> notAppParser
