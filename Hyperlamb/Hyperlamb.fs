module Hyperlamb.Program

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.ServerErrors

open FParsec

type Token = Abstraction | Application | Reference of int

type ExpI =
  | VarI of int
  | LamI of int * ExpI
  | AppI of ExpI * ExpI

type ExpII =
  | VarII of int
  | LamII of int * ExpI
  | AppII of ExpII * ExpII

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp


let unparse =
    let pstr s = "(" + s + ")"
    let rec unparse = function
        | Lam (p, b) -> "λ" + p + "." + unparse b
        | App (Lam (p, b), a) -> pstr (unparse (Lam (p, b))) + " " + argstr a
        | App (f, a) -> unparse f + " " + argstr a
        | Var s -> s
    and argstr = function
        | Var s -> s
        | t -> pstr (unparse t)
    unparse

let expmapper (ei : ExpI) (vars : string list) (scope : string list) = 
  let rec emapper (ei : ExpI) (scope : string list) =
    match ei with 
    | VarI n ->
      printfn "Lookup Var %d" (n - 1)
      let v : string = scope |> List.item (n - 1)
      Var v 
    | LamI (i, ei2) -> 
      printfn "Lookup Lam %d" i
      let v : string = vars |> List.item i
      Lam (v, emapper ei2 (v::scope))
    | AppI (ei1, ei2) -> App (emapper ei1 scope, emapper ei2 scope)
  emapper ei scope

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

let rec help tokens no =
   match tokens with
   | [] -> None
   | h::t ->
     match h with 
     | Reference n -> Some (VarI n, t)
     | Abstraction -> 
       help t (no + 1) |> Option.map (fun (e, r) -> LamI (no, e), r)
     | Application ->
       match help t no with
       | None -> None
       | Some (e1, r1) ->
         match help r1 no with
         | None -> None
         | Some (e2, r2) ->
           Some (AppI (e1, e2), r2)

// λx.λy.x -> LLR2
// λf.λx.f (f x)
// LLA[R2,A[R2,R1]]
// LLAR2AR2R1

let parse (p : Parser<Token list, unit>) (str : string) (vars : string list): string =
  printfn "input <%s>" str
  match run p str with
  | Success(result, _, _)   -> 
    match help result 0 with
    | Some (ei, ts) -> 
      let e = expmapper ei vars []   
      let s = unparse e
      printf "%s" s
      s
    | None ->
      sprintf "tokens good, expression bad"
  | Failure(errorMsg, _, _) -> errorMsg

let getVars (r : HttpRequest) : string list = 
  let defaults = ['a' .. 'z'] |> List.map (fun c -> string(c))
  if r.rawQuery.Length = 0 then
    defaults
  else
    let qps = r.query
    let qvars = qps |> List.map (fun (k, v) -> k) 
    qvars @ (defaults |> List.except qvars)

let handle lamb = request (fun r ->
  match r.header "Accept" with
  | Choice1Of2 x ->
    printfn "1: %s" x
  | Choice2Of2 x ->
    printfn "2: %s" x
  let vars = getVars r
  parse tokenParser lamb vars |> OK)

let app : WebPart = 
  choose [ 
      pathScan "/hyperlamb/%s" handle >=> Writers.setMimeType "text/plain; charset=utf-8"
      NOT_FOUND "nope" 
  ]
//  choose
//  [ pathScan "/hyperlamb/%s" (fun (exp) -> OK "lol" ) 
//    NOT_FOUND ]

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0
