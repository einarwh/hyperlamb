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
open Something
open Names
open NameRegister

type VarType = NamedLambdaVar | OrdinaryVar 


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
// zero (λf.λx.x)
// 2 (\f.\x.f (f x))
// 3 (\f.\x.f (f (f x)))

// succ λn.λf.λx.f (n f x)
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

//Cache-Control: no-store, must-revalidate
//Pragma: no-cache
//Expires: 0
  
let rec applyNamedLambdas vars expResult : ExpResult = 
  expResult

let createLink exp = 
  let scope = []
  let tokenString = tokenify scope exp |> toTokenString 
  let bounds = listBoundVariables exp
  let link = tokenString + toQueryString bounds
  link

let createTextHtmlResponse maybeExpResult =
  match maybeExpResult with 
  | Some { self = exp; next = maybeNext } ->
    let expResourceString = createLink exp
    printfn "Looking for registered name for token string %s" expResourceString
    let namedLambdas = lookupNamedLambdasByExactTokenString expResourceString
    let names = namedLambdas |> List.map (fun { name = n; lambda = _; encoded = _ } -> n)
    // If it is a named lambda, display the name.
    // Otherwise, allow user to provide a name for it.
    let showNamesDiv = 
      if names.IsEmpty then "<div />"
      else
        names |> List.map (fun n -> sprintf "<li>%s</li>" n) 
              |> String.concat "" 
              |> sprintf "<div>Names: <ul>%s</ul></div>"
    let addNameDiv = 
      sprintf "<div><form action=\"/names\" method=\"POST\">Add name:<br /><input type=\"text\" name=\"name\" /><input type=\"hidden\" name=\"lambda\" value=\"%s\" /><input type=\"submit\" value=\"Submit\"></form></div>" (unparse exp)
    match maybeNext with 
    | Some exp' ->
      let scope = []
      let tokenString = tokenify scope exp' |> toTokenString 
      let bounds = listBoundVariables exp'
      let nextLink = tokenString + toQueryString bounds
      let html = sprintf "<html><style>body { font-family: consolas; }</style><body><p>%s</p><p><a href=\"%s\">Next</a></p>%s%s<a href=\"/\">Home</a></body></html>" (unparse exp) nextLink showNamesDiv addNameDiv
      OK html >=> setHeader "Pragma" "no-cache"
              >=> setHeader "Content-Type" "text/html; charset=utf-8"
    | None ->
      let html = sprintf "<html><style>body { font-family: consolas; }</style><body><p>%s</p>%s%s<a href=\"/\">Home</a></body></html>" (unparse exp) showNamesDiv addNameDiv
      OK html >=> setHeader "Pragma" "no-cache"
              >=> setHeader "Content-Type" "text/html; charset=utf-8"
  | None ->
    "nope" |> OK

let toLambdaLink encodedLambdaString = 
  sprintf "/%s" encodedLambdaString

let handleGetLambda lamb = request (fun r ->
  printfn "handleGetLambda %s" lamb
  let acceptableMimeType = getPreferredMimeTypeFromRequest r
  let vars : (string * VarType) list = getVars r
  let namedLambdaNames = vars |> List.filter (fun (n, t) -> t = NamedLambdaVar) |> List.map (fun (n, t) -> n)
  let missingNamedLambdas = namedLambdaNames |> List.filter (fun n -> n |> (isNamedLambda >> not))
  if missingNamedLambdas.Length > 0 then 
    BAD_REQUEST "Missing named lambdas"
  else
    let namedLambdas = namedLambdaNames |> List.map (fun n -> n, lookupNamedLambdaByName n)
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
    match registerNamedLambda { name = name; lambdaString = lambda } with
    | FailedRegistrationDueToNameAlreadyRegistered ->
      sprintf "The name %s is already registered." name |> CONFLICT 
    | FailedRegistrationDueToInvalidLambda x ->
      x |> BAD_REQUEST
    | SuccessfulRegistration namedLambda ->
      let locationHeader = toLambdaLink namedLambda.encoded
      let refreshHeader = sprintf "0; url=%s" locationHeader
      CREATED "" >=> setHeader "location" locationHeader
                 >=> setHeader "refresh" refreshHeader
  | Choice2Of2 _, Choice1Of2 _ ->
    "Missing name." |> BAD_REQUEST
  | Choice1Of2 _, Choice2Of2 _ ->
    "Missing lambda." |> BAD_REQUEST
  | Choice2Of2 _, Choice2Of2 _ ->
    "Missing both name and lambda." |> BAD_REQUEST)

let handlePostUnnamedLambda = request (fun r -> 
  printfn "handlePostUnnamedLambda"
  let maybeLambda = r.formData "lambda"
  match maybeLambda with 
  | Choice1Of2 lambda ->
    match run expParser lambda with
    | Success(exp, _, _) ->
      printfn "success - got exp"
      let scope = []
      let tokenString = tokenify scope exp |> toTokenString 
      let bounds = listBoundVariables exp
      let link = tokenString + toQueryString bounds
      let location = sprintf "/%s" link
      let refresh = sprintf "0; url=%s" location
      CREATED "" >=> setHeader "location" location >=> setHeader "refresh" refresh
    | Failure(x,_,_) -> 
      printfn "nope"
      x |> BAD_REQUEST
  | _ ->
    "boom" |> BAD_REQUEST)

let handleGetNamedLambdas = request (fun r -> 
  printfn "handleGetNamedLambdas"
  let namedLambdas = match listAllNamedLambdas() with { namedLambdas = nls } -> nls
  let nameLinkItems = 
    namedLambdas 
    |> List.map (fun ({ name = name; lambda = _; encoded = ts }) -> (name, toLambdaLink ts))
    |> List.map (fun (name, link) -> sprintf "<a href=\"%s\">%s</a>" link name)
    |> List.map (fun a -> sprintf "<li>%s</li>" a)
  let nameLinks = 
    if nameLinkItems.IsEmpty then "" 
    else "<div class=\"names\"><p>Named lambdas</p><ul>" + String.concat "" nameLinkItems + "</ul></div>" 
  let html = sprintf "<html><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"/><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><style>body { font-family: consolas; }</style><body>%s<div><p>Add a new named lambda</p><form action=\"/names\" method=\"POST\">Name:<br /><input type=\"text\" name=\"name\" /><br />Lambda:<br /><input type=\"text\" name=\"lambda\" /><br /><br /><input type=\"submit\" value=\"Submit\"></form></div><a href=\"/\">Home</a></body></html>" nameLinks
  OK html)

let temporaryRedirect location = 
  setHeader "Location" location >=> response HTTP_307 [||]

let TEMPORARY_REDIRECT = temporaryRedirect

let seeOtherRedirect location = 
  setHeader "Location" location >=> response HTTP_303 [||]

let SEE_OTHER = seeOtherRedirect

let handleGetNamedLambda name = request (fun r -> 
  printfn "handleGetNamedLambda %s" name
  match lookupNamedLambda { name = name } with
  | FailedLookupDueToNameNotRegistered ->
    NOT_FOUND <| sprintf "The name %s isn't registered." name
  | SuccessfulLookup namedLambda ->    
    printfn "FOUND NAME %s" name
    let relativeUrl = toLambdaLink namedLambda.encoded
    printfn "URL %s" relativeUrl
    TEMPORARY_REDIRECT relativeUrl)

let handlePutNamedLambda name = request (fun r -> 
  printfn "handlePutNamedLambda %s" name
  let maybeName = r.formData "name"
  let maybeLambda = r.formData "lambda"
  match maybeName, maybeLambda with 
  | Choice1Of2 name, Choice1Of2 lambda ->
    match overwriteNamedLambda { name = name; lambdaString = lambda } with
    | FailedOverwriteDueToNameNotRegistered ->
      NOT_FOUND <| sprintf "The name %s isn't registered." name 
    | FailedOverwriteDueToInvalidLambda x ->
      BAD_REQUEST x
    | SuccessfulOverwrite namedLambda ->
      let locationHeader = toLambdaLink namedLambda.encoded
      FOUND locationHeader
  | Choice2Of2 _, Choice1Of2 _ ->
    "Missing name." |> BAD_REQUEST
  | Choice1Of2 _, Choice2Of2 _ ->
    "Missing lambda." |> BAD_REQUEST
  | Choice2Of2 _, Choice2Of2 _ ->
    "Missing both name and lambda." |> BAD_REQUEST)

let handleGetLambdaInput = request (fun r ->
  printfn "handleGetLambdaInput"
  let mimeType = getPreferredMimeTypeFromRequest r
  match mimeType with
  | TextHtml -> 
    let html = "<html><META HTTP-EQUIV=\"Pragma\" CONTENT=\"no-cache\"/><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/><style>body { font-family: consolas; }</style><body><p>Enter a lambda expression</p><form action=\"\" method=\"POST\"><input type=\"text\" name=\"lambda\" /><input type=\"submit\" value=\"Go\"></form><a href=\"/names\">Named lambdas</a><br/><a href=\"/\">Home</a></body>"
    html |> OK
  | _ -> NOT_ACCEPTABLE "cant")

let handleDeleteNamedLambda name = 
  printfn "handleDeleteNamedLambda %s" name
  match deleteNamedLambda { name = name } with
  | FailedDeleteDueToNameNotRegistered ->
    NOT_FOUND <| sprintf "The name %s isn't registered." name
  | SuccessfulDelete namedLambda ->
    NO_CONTENT

let app : WebPart = 
  choose [ 
      GET >=> choose [ path "/names" >=> handleGetNamedLambdas
                       pathScan "/names/%s" handleGetNamedLambda
                       path "/" >=> handleGetLambdaInput
                       pathScan "/%s" handleGetLambda
                       handleGetLambdaInput ]
      PUT >=> pathScan "/names/%s" handlePutNamedLambda
      POST >=> choose [ path "/names" >=> handlePostNamedLambda
                        handlePostUnnamedLambda ]
      DELETE >=> pathScan "/names/%s" handleDeleteNamedLambda
      NOT_FOUND "nope" 
  ]

[<EntryPoint>]
let main argv =
  let port = match argv with [| p |] -> (uint16 p) | _ -> uint16 8080
  let config =
    { defaultConfig with
        bindings = [ HttpBinding.create HTTP Net.IPAddress.Loopback port ]
        listenTimeout = TimeSpan.FromMilliseconds 3000. }
  startWebServer config app
  0
