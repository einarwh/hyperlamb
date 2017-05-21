module Parser 

open System
open Types
open FParsec

let expParser, expParserRef = createParserForwardedToRef<ExpN, unit>()

let varNameParser = 
  many1 lower |>> (List.toArray >> String)
  
let varParser = varNameParser |>> VarN

let nameParser = 
  (pchar '$' >>. varNameParser) |>> Name

let lamParser = 
        pipe2 ((pchar 'λ' <|> pchar '\\') >>. varNameParser) 
              (pchar '.' >>. expParser)
              (fun s e -> LamN (s, e))
let parParser = between (pchar '(') (pchar ')') expParser

let notAppParser = lamParser <|> nameParser <|> varParser <|> parParser

let appParser = 
  let applify defs =
    let leftest = List.head defs
    let restest = List.tail defs
    List.fold (fun acc e -> AppN (acc, e)) leftest restest 
  sepBy1 notAppParser (pchar ' ') |>> applify

do expParserRef := attempt appParser <|> notAppParser
