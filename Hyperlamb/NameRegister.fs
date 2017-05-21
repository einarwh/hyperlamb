module NameRegister

open FParsec

open Parser
open Something
open Names

type ListAllNamedLambdasResponse =
  { namedLambdas : NamedLambda list }

let listAllNamedLambdas ()
  : ListAllNamedLambdasResponse =
  let namedLambdas = listAllNamedLambdas() |> List.map (fun (n, nls) -> nls)
  { namedLambdas = namedLambdas }

type LookupNamedLambdaRequest = 
  { name : string }

type LookupNamedLambdaResponse = 
  | SuccessfulLookup of NamedLambda
  | FailedLookupDueToNameNotRegistered

let lookupNamedLambda (req : LookupNamedLambdaRequest) 
  : LookupNamedLambdaResponse = 
  let name = req.name
  match lookupNamedLambdaByName name with
  | None ->
    FailedLookupDueToNameNotRegistered
  | Some namedLambda ->
    SuccessfulLookup namedLambda

type RegisterNamedLambdaRequest = 
  { name : string
    lambdaString : string }

type RegisterNamedLambdaResponse = 
  | SuccessfulRegistration of NamedLambda
  | FailedRegistrationDueToNameAlreadyRegistered
  | FailedRegistrationDueToInvalidLambda of string

let registerNamedLambda (req : RegisterNamedLambdaRequest) 
  : RegisterNamedLambdaResponse = 
  let name = req.name
  match lookupNamedLambdaByName name with
  | Some _ ->
    FailedRegistrationDueToNameAlreadyRegistered
  | None ->
    //let lambdaString' = replaceNamedLambdas req.lambdaString
    match run expParser req.lambdaString with
    | Failure(x,_,_) -> 
      FailedRegistrationDueToInvalidLambda x  
    | Success(expn, _, _) ->
      let allNames = listAllNamedLambdas() |> (fun { namedLambdas = nls } -> nls)
      let exp = replaceNames allNames expn
      let scope = []
      let tokenString = tokenify scope exp |> toTokenString 
      let bounds = listBoundVariables exp
      let link = tokenString + toQueryString bounds
      let namedLambda = { name = name; lambda = exp; encoded = link }
      addNamedLambda namedLambda
      SuccessfulRegistration namedLambda

type OverwriteNamedLambdaRequest = 
  { name : string
    lambdaString : string }

type OverwriteNamedLambdaResponse = 
  | SuccessfulOverwrite of NamedLambda
  | FailedOverwriteDueToNameNotRegistered
  | FailedOverwriteDueToInvalidLambda of string

let overwriteNamedLambda (req: OverwriteNamedLambdaRequest)
  : OverwriteNamedLambdaResponse = 
  let name = req.name
  match lookupNamedLambdaByName name with
  | None ->
    FailedOverwriteDueToNameNotRegistered
  | Some _ ->
    match run expParser req.lambdaString with
    | Failure(x,_,_) -> 
      FailedOverwriteDueToInvalidLambda x  
    | Success(expn, _, _) ->
      let allNames = listAllNamedLambdas() |> (fun { namedLambdas = nls } -> nls)
      let exp = replaceNames allNames expn
      let scope = []
      let tokenString = tokenify scope exp |> toTokenString 
      let bounds = listBoundVariables exp
      let link = tokenString + toQueryString bounds
      let namedLambda = { name = req.name; lambda = exp; encoded = link }
      addNamedLambda namedLambda
      SuccessfulOverwrite namedLambda

type DeleteNamedLambdaRequest = 
  { name : string }

type DeleteNamedLambdaResponse = 
  | SuccessfulDelete of NamedLambda
  | FailedDeleteDueToNameNotRegistered

let deleteNamedLambda (req : DeleteNamedLambdaRequest) 
  : DeleteNamedLambdaResponse = 
  let name = req.name
  match lookupNamedLambdaByName name with
  | None ->
    FailedDeleteDueToNameNotRegistered
  | Some namedLambda ->
    SuccessfulDelete namedLambda
