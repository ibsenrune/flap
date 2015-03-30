module AstAnalyser
  open Ast
  open Symbols

  let rec freeVariables expr =
    match expr with
    | CstI(_) -> []
    | Var(id) -> id::[]
    | Let(p, lExpr, lBody) ->
        let freeVariablesInLExpr = freeVariables lExpr
        let freeVariablesInLBody = freeVariables lBody
        freeVariablesInLExpr @ (List.filter (fun x -> x <> p) freeVariablesInLBody)
    | LetFun(f, p, fBody, lBody) ->
        let freeVariablesInFunctionBody = freeVariables fBody
        let freeVariablesInLetBody = freeVariables lBody
        (List.filter (fun x -> x <> p) freeVariablesInFunctionBody) @ freeVariablesInLetBody
    | Op(operand1, op, operand2) ->
        freeVariables operand1 @ freeVariables operand2
    | Call(f, arg) -> freeVariables arg
        
        

  let rec freeIn (symbol : string) exprIn =
    match exprIn with
    | CstI(_) -> false
    | Var(v) -> symbol = v
    | Let(p, lExpr, lBody) -> 
        if(symbol = p) then false else freeIn symbol lExpr or freeIn symbol lBody
    

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
    | Op(e1, op, e2) -> Op(substitute env e1, op, substitute env e2)
    | Call(f, arg) -> 
      let substitutedArg = substitute env arg
      Call(f, substitutedArg)
      

