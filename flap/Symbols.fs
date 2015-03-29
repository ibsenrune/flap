module Symbols
  open Ast

  type 't environment = (string * 't) list

  let rec lookupOrSelf (env : Expr environment) x = 
    match env with
    | [] -> Var(x)
    | (symbol ,value)::rest -> if x=symbol then value else lookupOrSelf rest x

  let rec remove (env : Expr environment) x =
    match env with
    | [] -> []
    | (symbol, value)::rest -> if symbol = x then rest else (symbol, value) :: (remove rest x)