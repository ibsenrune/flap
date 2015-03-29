module AstAnalyser
  open Ast
  open Symbols

  let rec substitute (env : Expr environment) exprIn = 
    match exprIn with
    | CstI(_) -> exprIn
    | Var(v) -> lookupOrSelf env v
    | Let(p, lExpr, lBody) -> 
      let substitutedLExpr = substitute env lExpr
      let env' = remove env p
      let substitutedLBody = substitute env' lBody
      Let(p, substitutedLExpr, substitutedLBody)
    | LetFun(fName, pName, fBody, lBody) -> 
      let lBodyEnv = remove env fName
      let fBodyEnv = remove lBodyEnv pName
      let substitutedFBody = substitute fBodyEnv fBody
      let substitutedLBody = substitute lBodyEnv lBody
      LetFun(fName, pName, substitutedFBody, substitutedLBody)
      

