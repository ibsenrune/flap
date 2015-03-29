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
    | LetFun(f, p, fBody, iBody) ->
      let closure = Closure(f, p, fBody, env)
      let env' = (f, closure)::env
      eval iBody env'
    | Call(id, arg) -> 
      let (f,p,fBody,fEnv) = 
        match lookup id with
        | Closure(f,p,fBody,fEnv) -> (f,p,fBody,fEnv)
        | _ -> failwith "Cannot call non-func value"
      let argValue = eval arg env
      eval fBody ((p,Int(argValue))::fEnv)
    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = eval lhs env, eval rhs env
      match op with
      | "+" -> lhsv + rhsv
      | "-" -> lhsv - rhsv
      | "*" -> lhsv * rhsv
      | "/" -> lhsv / rhsv
