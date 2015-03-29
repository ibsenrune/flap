module Interpreter
  open Ast

  type 't environment = (string * 't) list

  type value = 
  | Int of int
  | Closure of string * string * Expr * value environment

  let rec eval e (env : value environment) =
    let lookup s = List.find (fun (x,_) -> x = s) env |> snd
    match e with
    | CstI(i) -> i
    | Var(s) -> 
        match lookup s with
        | Int(i) -> i
        | _ -> failwith "Lookup returned unexpected value"
    | Let(s, elhs, erhs) -> 
      let vlhs = eval elhs env
      let env' = (s,Int(vlhs))::env
      eval erhs env'
    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = eval lhs env, eval rhs env
      match op with
      | "+" -> lhsv + rhsv
      | "-" -> lhsv - rhsv
      | "*" -> lhsv * rhsv
      | "/" -> lhsv / rhsv
