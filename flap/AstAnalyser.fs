module AstAnalyser
  open Ast
  open Symbols

  let rec freeVariables expr =
    match expr with
    | CstI(_) -> []
    | CstB(_) -> []
    | StringC(_) -> []
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
    | If(bExpr, tExpr, fExpr) -> 
        freeVariables bExpr @ freeVariables tExpr @ freeVariables fExpr
        
        
  let rec private freeIn (symbol : string) exprIn =
    (List.exists (fun x -> x = symbol)) (freeVariables exprIn)
    

  let rec substitute (uniqueVarGenerator : unit -> string) (env : Expr environment) exprIn = 
    let substituteInner = substitute uniqueVarGenerator
    match exprIn with
    | CstI(_) -> exprIn
    | CstB(_) -> exprIn
    | StringC(_) -> exprIn
    | Var(v) -> lookupOrSelf env v
    | Let(p, lExpr, lBody) -> 
      let substitutedLExpr = substituteInner env lExpr
      let newP = uniqueVarGenerator()
      let env' = (p, Var(newP)) :: remove env p
      let substitutedLBody = substituteInner env' lBody
      Let(newP, substitutedLExpr, substitutedLBody)
    | LetFun(fName, pName, fBody, lBody) -> 
      let newF = uniqueVarGenerator()
      let newP = uniqueVarGenerator()
      let fBodyEnv = (pName, Var(newP)) :: (fName, Var(newF)) :: remove (remove env pName) fName
      let lBodyEnv = (fName, Var(newF)) :: remove env fName      
      let substitutedFBody = substitute uniqueVarGenerator fBodyEnv fBody
      let substitutedLBody = substitute uniqueVarGenerator lBodyEnv lBody
      LetFun(newF, newP, substitutedFBody, substitutedLBody)
    | Op(e1, op, e2) -> Op(substitute uniqueVarGenerator env e1, op, substitute uniqueVarGenerator env e2)
    | Call(f, arg) -> 
      let substitutedArg = substitute uniqueVarGenerator env arg
      Call(f, substitutedArg)
    | If(bExpr, tExpr, fExpr) -> 
      If(substituteInner env bExpr, substituteInner env tExpr, substituteInner env fExpr)
      

