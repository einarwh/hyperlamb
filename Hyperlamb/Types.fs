module Types

type Token = Abstraction | Application | Reference of int

type TokenI = AbstractionI of int | ApplicationI | ReferenceI of int

type ExpI =
  | VarI of int
  | LamI of int * ExpI
  | AppI of ExpI * ExpI

type Exp =
  | Var of string
  | Lam of string * Exp
  | App of Exp * Exp
