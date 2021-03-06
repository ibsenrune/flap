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
    | StringC(s) -> e
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
      let fClosure = lookup id
      match fClosure with
      | Closure(f,p,fBody,fEnv) -> 
        let argValue = eval arg env
        let fBodyEnv = (f, fClosure)::fEnv
        eval fBody ((p,ExprValue(argValue, []))::fBodyEnv)
      | _ -> failwith "Cannot call non-func value"
      

    | Op(lhs, op, rhs) -> 
      let lhsv,rhsv = eval lhs env, eval rhs env
      match lhsv, rhsv with
      | CstI(lhsi), CstI(rhsi) -> 
        match op with
        | "+" -> CstI(lhsi + rhsi)
        | "-" -> CstI(lhsi - rhsi)
        | "*" -> CstI(lhsi * rhsi)
        | "/" -> CstI(lhsi / rhsi)
        | "=" -> CstB(lhsi = rhsi)
        | _ -> failwith (sprintf "Operator %s cannot be applied to operands of type int" op)
      | CstB(b1), CstB(b2) ->
        match op with
        | "=" -> CstB(b1 = b2)
        | _ -> failwith (sprintf "Cannot apply operator %s to operands of type bool" op)
      | StringC(s1), StringC(s2) -> 
        match op with
        | "+" -> StringC(s1 + s2)
        | "=" -> CstB(s1 = s2)
      | _ -> failwith (sprintf "Cannot apply operator %s to expressions" op)

    | If(cExpr, tExpr, fExpr) ->
      let conditionValue = eval cExpr env
      match conditionValue with
      | CstB(b) -> if b then eval tExpr env else eval fExpr env
      | _ -> failwith "Expression in If construct did not evaluate to a boolean value"
