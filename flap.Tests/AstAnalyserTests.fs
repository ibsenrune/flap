module AstAnalyserTests
  open Ast
  open AstAnalyser
  open Xunit
  open Xunit.Extensions
  open Ploeh.AutoFixture.Xunit

  let uniqueVariables = ["unique1"; "unique2"; "unique3"; "unique4"]
  let uniqueGenerator () = 
    let enumerator = (List.toSeq uniqueVariables).GetEnumerator()
    fun () -> enumerator.MoveNext() |> ignore; enumerator.Current

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoIntegerConstDoesNothing (p : string) (expr : Expr) i =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let cst = CstI(i)
    let env = [(p,expr)]

    let actual = substitute env cst

    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoStringConstDoesNothing (p : string) (expr : Expr) s =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let cst = StringC(s)
    let env = [(p,expr)]

    let actual = substitute env cst

    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoBooleanConstDoesNothing (p : string) (expr : Expr) b =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let cst = CstB(b)
    let env = [(p,expr)]

    let actual = substitute env cst

    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsNotSubstitutionVarDoesNothing 
        (p : string) (var : string) (expr : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let varExpr = Var(var)
    let env = [(p,expr)]

    let actual = substitute env varExpr

    Assert.NotEqual(p, var);
    Assert.Equal(varExpr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsSubstitutionVarResultsInSubstitedExpression
        (p : string) (expr : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let varExpr = Var(p)
    let env = [(p,expr)]

    let actual = substitute env varExpr

    Assert.Equal(expr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoLetDoesNothingWhenSubstituteVariableIsBoundByTheLet 
        (p : string) (subExpr : Expr) (letExpr : Expr) (letBody : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let expr = Let(p, letExpr, letBody)
    let env = [(p, subExpr)]

    let actual = substitute env expr

    Assert.Equal(Let("unique1", letExpr, letBody), actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoLetSubstituesIntoLetExprWhenSubstitutionVarIsBoundVariable
        (p : string) (subExpr : Expr) (letBody : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letExpr = Let(p, Var(p), letBody)
    let env = [(p, subExpr)]

    let actual = substitute env letExpr

    Assert.Equal(Let("unique1", subExpr, letBody), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoFunctionBody
    (f : string) (p : string) (fBodyVar : string) (subExpr : Expr) (letBody : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letFun = LetFun(f, p, Var(fBodyVar), letBody)
    let substitutionEnv = [(fBodyVar, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.NotEqual(p, fBodyVar)
    Assert.Equal(LetFun("unique1", "unique2", subExpr, letBody), actual)

  
  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunDoesNotSubstituteIntoFunctionBodyWhenParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBodyVar : string) (subExpr : Expr) (letBody : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letFun = LetFun(fName, pName, Var(pName), letBody)
    let substitutionEnv = [(pName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun("unique1", "unique2", Var("unique2"), letBody), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoLetBodyWhenParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) (lBodyVar : string) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letFun = LetFun(fName, pName, fBody, Var(lBodyVar))
    let substitutionEnv = [(lBodyVar, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun("unique1", "unique2", fBody, subExpr), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunDoesNotSubstituteIntoLetBodyWhenFunctionIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letFun = LetFun(fName, pName, fBody, Var(fName))
    let substitutionEnv = [(fName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun("unique1", "unique2", fBody, Var("unique1")), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoLetBodyWhenFunctionParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let letFun = LetFun(fName, pName, fBody, Var(pName))
    let substitutionEnv = [(pName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun("unique1", "unique2", fBody, subExpr), actual)

  [<Theory>]
  [<InlineAutoData("+")>]
  [<InlineAutoData("-")>]
  [<InlineAutoData("*")>]
  [<InlineAutoData("/")>]
  let SubstituteIntoOpSubstitutesIntoOperands op substitutionVar subExpr =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let expr = Op(Var(substitutionVar), op, Var(substitutionVar))
    let env = (substitutionVar, subExpr)::[]

    let actual = substitute env expr

    Assert.Equal(Op(subExpr, op, subExpr), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoCallSubstitutesIntoArgumentExpression 
    (fName : string) (argVar : string) (subExpr : Expr) =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let call = Call(fName, Var(argVar))
    let env = (argVar, subExpr)::[]

    let actual = substitute env call

    Assert.Equal(Call(fName, subExpr), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoIfSubstitutesIntoAllThreeExpressions subExpr var =
    let substitute = AstAnalyser.substitute (uniqueGenerator())
    let ifExpr = If(Var(var), Var(var), Var(var))
    let env = (var, subExpr)::[]

    let actual = substitute env ifExpr

    Assert.Equal(If(subExpr, subExpr, subExpr), actual)


  [<Theory>]
  [<AutoData>]
  let InIntegerConstNothingIsFree i =
    let expr = CstI(i)

    let actual = freeVariables expr

    Assert.Equal([], actual)

  [<Theory>]
  [<AutoData>]
  let InBooleanConstNothingIsFree b =
    let expr = CstB(b)

    let actual = freeVariables expr

    Assert.Equal([], actual)

  [<Theory>]
  [<AutoData>]
  let InStringConstNothingIsFree s =
    let expr = StringC(s)

    let actual = freeVariables expr

    Assert.Equal([], actual)

  [<Theory>]
  [<AutoData>]
  let InVarTheVarIsFree var =
    let expr = Var(var)

    let actual = freeVariables expr

    Assert.Equal(var::[], actual)

  [<Theory>]
  [<AutoData>]
  let InLetFreeVariablesInLetBodyAreFreeWhenNotBoundByLet p var i1 =
    let letExpr = Let(p, CstI(i1), Var(var))

    let actual = freeVariables letExpr

    Assert.Equal(var::[], actual)

  
  [<Theory>]
  [<AutoData>]
  let InLetFreeVariablesInLetBodyAreNotFreeWhenBoundByLet var i1 =
    let letExpr = Let(var, CstI(i1), Var(var))

    let actual = freeVariables letExpr

    Assert.Equal([], actual)

  
  [<Theory>]
  [<AutoData>]
  let InLetFreeVariablesInLetExprAreFree p var i1 =
    let letExpr = Let(p, Var(var), CstI(i1))

    let actual = freeVariables letExpr

    Assert.Equal(var::[], actual)


  [<Theory>]
  [<AutoData>]
  let InLetFreeVariablesInLetExprAreFreeEvenIfEqualToParameter var i1 =
    let letExpr = Let(var, Var(var), CstI(i1))

    let actual = freeVariables letExpr

    Assert.Equal(var::[], actual)


  [<Theory>]
  [<AutoData>]
  let InLetFunFreeVariablesInLetBodyAreFreeWhenNotBoundByLet f p var i1 =
    let letFun = LetFun(f, p, CstI(i1), Var(var))

    let actual = freeVariables letFun

    Assert.Equal(var::[], actual)


  [<Theory>]
  [<AutoData>]
  let InCallFreeVariablesAreFreeInArg f var =
    let call = Call(f, Var(var))

    let actual = freeVariables call

    Assert.Equal(var::[], actual)
  
  [<Theory>]
  [<AutoData>]
  let InOpFreeVariablesAreUnionOfSubexpressionsFreeVariables var1 var2 =
    let op = Op(Var(var1), "+", Var(var2))

    let actual = freeVariables op

    Assert.Equal(var1::var2::[], actual)


  [<Theory>]
  [<AutoData>]
  let InLetFunFreeVariablesInLetBodyAreFreeEvenWhenBoundByLetFun f var i1 =
    let letFun = LetFun(f, var, CstI(i1), Var(var))

    let actual = freeVariables letFun

    Assert.Equal(var::[], actual)

  
  [<Theory>]
  [<AutoData>]
  let InLetFunFreeVariablesInLetExprAreFreeWhenNotBoundByparameter f p var i1 =
    let letFun = LetFun(f, p, Var(var), CstI(i1))

    let actual = freeVariables letFun

    Assert.NotEqual(p, var)
    Assert.Equal(var::[], actual)

  
  [<Theory>]
  [<AutoData>]
  let InLetFunFreeVariablesInLetExprAreNotFreeWhenBoundByparameter f p var i1 =
    let letFun = LetFun(f, var, Var(var), CstI(i1))

    let actual = freeVariables letFun

    Assert.NotEqual(p, var)
    Assert.Equal([], actual)


  [<Theory>]
  [<AutoData>]
  let InLetFunFreeVariablesAreFreeVariablesFromSubexpressions f p var1 var2 =
    let letExpr = LetFun(f, p, Var(var1), Var(var2))

    let actual = freeVariables letExpr

    Assert.NotEqual(p, var1);
    Assert.NotEqual(p, var2);
    Assert.Equal(var1::var2::[], actual)




  [<Theory>]
  [<AutoData>]
  let InLetFreeVariablesAreFreeInSubexpressoins p var1 var2 =
    let letExpr = Let(p, Var(var1), Var(var2))

    let actual = freeVariables letExpr

    Assert.Equal(var1::var2::[], actual)


  [<Theory>]
  [<AutoData>]
  let InIfTheFreeVariablesAreTheFreeVariablesFromTheThreeSubexpressions f1 f2 f3 =
    let ifExpr = If(Var(f1), Var(f2), Var(f3))

    let actual = freeVariables ifExpr

    Assert.Equal(f1::f2::f3::[], actual)
