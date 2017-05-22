module ParseExpThings

open Types
open Eval
open TokenParser
open FParsec


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
  printfn "parseExp <%s>" str
  match run p str with
  | Success(result, _, _) -> 
    let tokens = tokenMapper result 0
    match help tokens with
    | Some (ei, ts) ->      
      let boundVariableCount = countLambdas result
      let bound = vars |> List.take boundVariableCount |> List.map (fun (n, t) -> n)
      let free = vars |> List.skip boundVariableCount |> List.map (fun (n, t) -> n)
      let e = expmapper ei bound free [] 
      Some e
    | None -> None
  | Failure(errorMsg, _, _) -> 
    None

let parseExpResult (p : Parser<Token list, unit>) (str : string) (vars : (string * VarType) list)
  : ExpResult option =
  printfn "parseExpResult <%s>" str
  match parseExp p str vars with
  | Some exp ->
    let exp' = reduce exp
    let result = 
      let maybeNext = 
        match reduce exp with
        | Next exp' -> Some exp'
        | Normal -> None
      { self = exp; next = maybeNext }
    Some result
  | None -> None