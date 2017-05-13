module Hyperlamb.Program

open System
open System.Collections.Generic

open Suave
open Suave.Response
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Redirection
open Suave.RequestErrors
open Suave.ServerErrors
open Writers

open FParsec

open Types
open Parser
open Eval

type VarType = NamedLambdaVar | OrdinaryVar 

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

let getVariableName (scope : string list) (free : string list) (n : int) =
  if n < scope.Length then
    List.item n scope
  else 
    List.item (n - List.length scope) free
  
let expmapper (ei : ExpI) 
              (bound : string list) 
              (free : string list) 
              (scope : string list) = 
  let rec emapper (ei : ExpI) (scope : string list) =
    match ei with 
    | VarI n ->
      printfn "Lookup Var %d" (n - 1)
      let v : string = getVariableName scope free (n - 1)
      Var v 
    | LamI (i, ei2) -> 
      printfn "Lookup Lam %d" i
      let v : string = bound |> List.item i
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

type LambResult<'T> = Yay of 'T | Nay of string 

let rec tokenMapper tokens lambdaIndex = 
  match tokens with
  | [] -> []
  | h::t -> 
    match h with 
    | Reference n -> ReferenceI n :: (tokenMapper t lambdaIndex)
    | Abstraction -> AbstractionI lambdaIndex :: (tokenMapper t (lambdaIndex + 1))
    | Application -> ApplicationI :: (tokenMapper t lambdaIndex)

let rec help tokens =
   match tokens with
   | [] -> None
   | h::t ->
     match h with 
     | ReferenceI n -> Some (VarI n, t)
     | AbstractionI ix -> 
       help t |> Option.map (fun (e, r) -> LamI (ix, e), r)
     | ApplicationI ->
       match help t with
       | None -> None
       | Some (e1, r1) ->
         match help r1 with
         | None -> None
         | Some (e2, r2) ->
           Some (AppI (e1, e2), r2)

// λx.λy.x -> LLR2
// λf.λx.f (f x)
// LLA[R2,A[R2,R1]]
// LLAR2AR2R1
// zero (\f.\x.x)
// 2 (\f.\x.f (f x))
// 3 (\f.\x.f (f (f x)))

// succ \n.\f.\x.f ((n f) x)
// add \a.\b.a (\n.\f.\x.f (n f x)) b

// succ LLLAR2AAR3R2R1?n&f&x
// zero LLR1?f&x
// zero λf.λx.x

// (succ zero)
// ALLLAR2AAR3R2R1LLR1?n&f&x&a&b
// ALLLAR2AAR3R2R1ALLLAR2AAR3R2R1LLR1?n&f&x&a&b

// exp = (λn.λf.λx.f (n f x)) (λa.λb.b)
// exp' = λf.λx.f ((λa.λb.b) f x)
// LLAR2AALLR1R2R1?f&x&a&b

// exp'' = λf.λx.f ((λb.b) x)
// LLAR2ALR1R1?f&x&b

type ExpResult = 
  { self : Exp 
    next : Exp option }

let rec countLambdas tokens =
   match tokens with
   | [] -> 0
   | h::t ->
     let n = countLambdas t
     match h with
     | Abstraction -> 1 + n
     | _ -> n
  
let parseExpI (p : Parser<Token list, unit>) (str : string): ExpI option =
  match run p str with
  | Success(result, _, _) -> 
    let lambdas = countLambdas result
    let tokens = tokenMapper result 0
    help tokens |> Option.map (fun (ei, ts) -> ei)
  | Failure(errorMsg, _, _) -> None

let parseExp (p : Parser<Token list, unit>) (str : string) (vars : (string * VarType) list)
  : Exp option =
  printfn "input <%s>" str
  match run p str with
  | Success(result, _, _) -> 
    let tokens = tokenMapper result 0
    match help tokens with
    | Some (ei, ts) ->      
      let boundVariableCount = countLambdas result
      let bound = vars |> List.take boundVariableCount |> List.map (fun (n, t) -> n)
      let free = vars |> List.skip boundVariableCount |> List.map (fun (n, t) -> n)
      printfn "All vars: %A" vars
      printfn "Bound vars: %A" bound
      printfn "Free vars: %A" free
      let e = expmapper ei bound free [] 
      Some e
    | None -> None
  | Failure(errorMsg, _, _) -> 
    None

let parseExpResult (p : Parser<Token list, unit>) (str : string) (vars : (string * VarType) list)
  : ExpResult option =
  printfn "input <%s>" str
  let maybeExp = parseExp p str vars
  match maybeExp with
  | Some exp ->
    let exp' = reduce exp
    printfn "exp = %s" (unparse exp)
    printfn "exp' = %s" (unparse exp')
    let result = 
      if exp = exp' then
        { self = exp; next = None }
      else
        { self = exp; next = Some exp' }
    Some result 
  | None -> None


let getVars (r : HttpRequest) : (string * VarType) list = 
  let defaults = ['a' .. 'z'] |> List.map (fun c -> string(c))
  if r.rawQuery.Length = 0 then
    defaults |> List.map (fun n -> n, OrdinaryVar)
  else
    let qps = r.query
    let qvars = qps |> List.map (fun (k, v) -> k, match v with Some "name" -> NamedLambdaVar | _ -> OrdinaryVar) 
    let qvarnames = qvars |> List.map (fun (n, t) -> n)
    let additional = defaults |> List.except qvarnames
    let vars = qvars @ (additional |> List.map (fun n -> n, OrdinaryVar))
    vars

type AcceptableMimeType = TextPlain | TextHtml | ImagePng | Hal

let getPreferredMimeTypeFromAcceptHeader (acceptHeader : string) =
  if acceptHeader.Contains("text/plain") then TextPlain
  else if acceptHeader.Contains("text/html") then TextHtml
  else if acceptHeader.Contains("image/png") then ImagePng
  else if acceptHeader.Contains("application/hal") then Hal
  else TextPlain

let getPreferredMimeTypeFromRequest (request : HttpRequest) : AcceptableMimeType = 
  match request.header "Accept" with
  | Choice1Of2 acceptHeader ->
    getPreferredMimeTypeFromAcceptHeader acceptHeader
  | Choice2Of2 x ->
    TextPlain
  
let createTextPlainResponse maybeExp =
  match maybeExp with 
  | Some exp ->
    let response = unparse exp |> OK 
    response >=> Writers.setMimeType "text/plain; charset=utf-8"
  | None ->
    "nope" |> OK

let toQueryString (bounds : string list) = 
  if bounds.Length = 0 then ""
  else bounds |> String.concat "&" |> sprintf "?%s"

let createTextHtmlResponse maybeExpResult =
  match maybeExpResult with 
  | Some { self = exp; next = Some exp' } ->
    let scope = []
    let tokenString = tokenify scope exp' |> toTokenString 
    let bounds = listBoundVariables exp'
    let nextLink = tokenString + toQueryString bounds
    printfn "%A" bounds
    sprintf "<html><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"/><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><style>body { font-family: consolas; }</style><body><p>%s</p><p><a href=\"%s\">Next</a></p></body></html>" (unparse exp) nextLink |> OK
  | Some { self = exp; next = None } ->
    let response = (sprintf "<html><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"/><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><style>body { font-family: consolas; }</style><body><p>%s</p></body>" (unparse exp) |> OK)
    response >=> Writers.setMimeType "text/html"
  | None ->
    "nope" |> OK

// 
//Cache-Control: no-store, must-revalidate
//Pragma: no-cache
//Expires: 0

type NamedLambda = 
  { name: string
    lambda: Exp
    encoded: string }

let nameMap = 
  let m = Dictionary<string, NamedLambda>()
  m.Add("id", { name = "id"; lambda = Lam ("x", Var "x"); encoded = "LR1?x" })
  m
  
let rec applyNamedLambdas vars expResult : ExpResult = 
  expResult

let handleGetLambda lamb = request (fun r ->
  let acceptableMimeType = getPreferredMimeTypeFromRequest r
  let vars : (string * VarType) list = getVars r
  let namedLambdaNames = vars |> List.filter (fun (n, t) -> t = NamedLambdaVar) |> List.map (fun (n, t) -> n)
  let missingNamedLambdas = namedLambdaNames |> List.filter (fun n -> n |> (nameMap.ContainsKey >> not))
  if missingNamedLambdas.Length > 0 then 
    BAD_REQUEST "Missing named lambdas"
  else
    let namedLambdas = namedLambdaNames |> List.map (fun n -> n, nameMap.Item(n))
    let maybeExpRes = parseExpResult tokenParser lamb vars
    let maybeExpResult = maybeExpRes |> Option.map (fun er -> applyNamedLambdas namedLambdas er)
    match acceptableMimeType with 
    | TextPlain -> 
      createTextPlainResponse (maybeExpResult |> Option.map (fun { self = e; next = _} -> e))
    | TextHtml ->
      createTextHtmlResponse maybeExpResult)

let handlePostNamedLambda = request (fun r -> 
  printfn "handlePostNamedLambda"
  let maybeName = r.formData "name"
  let maybeLambda = r.formData "lambda"
  match maybeName, maybeLambda with 
  | Choice1Of2 name, Choice1Of2 lambda ->
    printfn "provided name %s" name
    printfn "provided lambda %s" lambda
    if nameMap.ContainsKey(name) then 
      sprintf "The name %s is already registered." name |> CONFLICT 
    else 
      match run expParser lambda with
      | Success(exp, _, _) ->
        let scope = []
        let tokenString = tokenify scope exp |> toTokenString 
        let bounds = listBoundVariables exp
        let link = tokenString + toQueryString bounds
        nameMap.Add(name, { name = name; lambda = exp; encoded = link })
        let location = sprintf "/hyperlamb/%s" link
        printfn "Location for %s is %s" name location
        let refresh = sprintf "0; url=%s" location
        CREATED "" >=> setHeader "location" location >=> setHeader "refresh" refresh
      | Failure(x,_,_) -> 
        x |> BAD_REQUEST
  | _ ->
    "boom" |> BAD_REQUEST)

let handleGetNames = request (fun r -> 
  let html = "<html><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"/><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><style>body { font-family: consolas; }</style><body><form action=\"/hyperlamb\" method=\"POST\">Name:<br /><input type=\"text\" name=\"name\" /><br />Lambda:<br /><input type=\"text\" name=\"lambda\" /><br /><br /><input type=\"submit\" value=\"Submit\"></form></body>" 
  OK html)

let temporaryRedirect location = 
  setHeader "Location" location >=> response HTTP_307 [||]

let handleGetName name = request (fun r -> 
  if nameMap.ContainsKey(name) then
    let { name = _; lambda = _; encoded = tokenString } = nameMap.Item(name)
    printfn "%s" r.host
    printfn "FOUND NAME %s" name
    let relativeUrl = sprintf "/hyperlamb/%s" tokenString
    printfn "URL %s" relativeUrl
    temporaryRedirect relativeUrl
  else
    sprintf "The name %s isn't registered." name |> NOT_FOUND)

let handlePutNamedLambda name =
  request (fun r -> 
  printfn "handlePutNamedLambda"
  let maybeName = r.formData "name"
  let maybeLambda = r.formData "lambda"
  match maybeName, maybeLambda with 
  | Choice1Of2 name, Choice1Of2 lambda ->
    printfn "provided name %s" name
    printfn "provided lambda %s" lambda
    if nameMap.ContainsKey(name) then 
      nameMap.Remove(name) |> ignore
      match run expParser lambda with
      | Success(exp, _, _) ->
        let scope = []
        let tokenString = tokenify scope exp |> toTokenString 
        let bounds = listBoundVariables exp
        let link = tokenString + toQueryString bounds
        nameMap.Add(name, { name = name; lambda = exp; encoded = link })
        let location = sprintf "/hyperlamb/%s" link
        printfn "Location for %s is %s" name location
        let refresh = sprintf "0; url=%s" location
        CREATED "" >=> setHeader "location" location >=> setHeader "refresh" refresh
      | Failure(x,_,_) -> 
        x |> BAD_REQUEST
    else
      sprintf "The name %s isn't registered." name |> NOT_FOUND 
  | _ ->
    "boom" |> BAD_REQUEST)

let handleDeleteName name = 
  printfn "handleDeleteName %s" name
  if nameMap.Remove(name) then
    NO_CONTENT
  else 
    sprintf "The name %s isn't registered." name |> NOT_FOUND
let app : WebPart = 
  choose [ 
      GET >=> choose [ path "/hyperlamb/names" >=> handleGetNames
                       pathScan "/hyperlamb/names/%s" handleGetName
                       pathScan "/hyperlamb/%s" handleGetLambda ]
      PUT >=> pathScan "/hyperlamb/names/%s" handlePutNamedLambda
      POST >=> path "/hyperlamb" >=> handlePostNamedLambda
      DELETE >=> pathScan "/hyperlamb/names/%s" handleDeleteName
      NOT_FOUND "nope" 
  ]

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0
