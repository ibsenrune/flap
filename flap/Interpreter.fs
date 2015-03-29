module Interpreter
  open Ast

  type 't environment = (string * 't) list

  type value = 
  | Int of int
  | Closure of string * string * Expr * value environment

  let rec eval e (env : value environment) =
    let lookup s = List.find (fun (x,_) -> x = s) env |> snd
    match e with
    | CstI(i) -> Int(i)
    | Var(s) -> lookup s
    | Let(s, elhs, erhs) -> 
      let vlhs = eval elhs env
      let env' = (s,vlhs)::env
      eval erhs env'
    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = 
        match eval lhs env, eval rhs env with
        | (Int(li),Int(ri)) -> (li,ri) 
        | _ -> failwith "Value does not support operator"
      match op with
      | "+" -> Int(lhsv + rhsv)
      | "-" -> Int(lhsv - rhsv)
      | "*" -> Int(lhsv * rhsv)
      | "/" -> Int(lhsv / rhsv)
