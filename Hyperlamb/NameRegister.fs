module NameRegister

open FParsec

open Parser
open Something
open Names

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
  match run expParser req.lambdaString with
  | Failure(x,_,_) -> 
    FailedRegistrationDueToInvalidLambda x  
  | Success(exp, _, _) ->
    let scope = []
    let tokenString = tokenify scope exp |> toTokenString 
    let bounds = listBoundVariables exp
    let link = tokenString + toQueryString bounds
    let namedLambda = { name = req.name; lambda = exp; encoded = link }
    addNamedLambda namedLambda
    SuccessfulRegistration namedLambda

type OverwriteNamedLambdaRequest = 
  { name : string
    lambdaString : string }

type OverwriteNamedLambdaResponse = 
  | SuccessfulOverwrite of NamedLambda
  | FailedOverwriteDueToNameNotRegistered
  | FailedOverwriteDueToInvalidLambda of string

let overwriteNamedLamba (req: OverwriteNamedLambdaRequest)
  : OverwriteNamedLambdaResponse = 
