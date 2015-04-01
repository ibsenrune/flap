﻿module Interpreter
  open Ast
  open Symbols

  type value = 
  | ExprValue of Ast.Expr * value environment
  | Closure of string * string * Expr * value environment

  let rec eval e (env : value environment) =
    let lookup s = List.find (fun (x,_) -> x = s) env |> snd
    match e with
    | CstI(i) -> e
    | CstB(b) -> e

    | Var(s) -> 
        match lookup s with
        | ExprValue(expr, env) -> eval expr env
        | _ -> failwith "Lookup returned unexpected value"

    | Let(s, elhs, erhs) -> 
      let env' = (s,ExprValue(elhs, env))::env
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
      eval fBody ((p,ExprValue(argValue, []))::fEnv)

    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = eval lhs env, eval rhs env
      match lhsv, rhsv with
      | (CstI(lhsi), CstI(rhsi)) -> 
        match op with
        | "+" -> CstI(lhsi + rhsi)
        | "-" -> CstI(lhsi - rhsi)
        | "*" -> CstI(lhsi * rhsi)
        | "/" -> CstI(lhsi / rhsi)
      | _ -> failwith (sprintf "Cannot apply operator %s to expressions" op)
