module Interpreter
  open Ast

  let rec eval e (env : (string*int) list) =
    let lookup s = List.find (fun (x,_) -> x = s) env |> snd
    match e with
    | CstI(i) -> i
    | Var(s) -> lookup s
    | Let(s, elhs, erhs) -> 
      let vlhs = eval elhs env
      let env' = (s,vlhs)::env
      eval erhs env'
    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = eval lhs env, eval rhs env
      match op with
      | "+" -> lhsv + rhsv
      | "-" -> lhsv - rhsv
      | "*" -> lhsv * rhsv
      | "/" -> lhsv / rhsv
