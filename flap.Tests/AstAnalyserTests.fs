module AstAnalyserTests
  open Ast
  open AstAnalyser
  open Xunit
  open Xunit.Extensions
  open Ploeh.AutoFixture.Xunit

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoConstDoesNothing (p : string) (expr : Expr) i =
    let cst = CstI(i)
    let env = [(p,expr)]

    let actual = substitute env cst

    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsNotSubstitutionVarDoesNothing 
        (p : string) (var : string) (expr : Expr) =
    let varExpr = Var(var)
    let env = [(p,expr)]

    let actual = substitute env varExpr

    Assert.NotEqual(p, var);
    Assert.Equal(varExpr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsSubstitutionVarResultsInSubstitedExpression
        (p : string) (expr : Expr) =
    let varExpr = Var(p)
    let env = [(p,expr)]

    let actual = substitute env varExpr

    Assert.Equal(expr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoLetDoesNothingWhenSubstituteVariableIsBoundByTheLet 
        (p : string) (subExpr : Expr) (letExpr : Expr) (letBody : Expr) =
    let letExpr = Let(p, letExpr, letBody)
    let env = [(p, subExpr)]

    let actual = substitute env letExpr

    Assert.Equal(letExpr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoLetSubstituesIntoLetExprWhenSubstitutionVarIsBoundVariable
        (p : string) (subExpr : Expr) (letBody : Expr) =
    let letExpr = Let(p, Var(p), letBody)
    let env = [(p, subExpr)]

    let actual = substitute env letExpr

    Assert.Equal(Let(p, subExpr, letBody), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoFunctionBody
    (f : string) (p : string) (fBodyVar : string) (subExpr : Expr) (letBody : Expr) =
    let letFun = LetFun(f, p, Var(fBodyVar), letBody)
    let substitutionEnv = [(fBodyVar, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.NotEqual(p, fBodyVar)
    Assert.Equal(LetFun(f, p, subExpr, letBody), actual)

  
  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunDoesNotSubstitutesIntoFunctionBodyWhenParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBodyVar : string) (subExpr : Expr) (letBody : Expr) =
    let letFun = LetFun(fName, pName, Var(pName), letBody)
    let substitutionEnv = [(pName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(letFun, actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoLetBodyWhenParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) (lBodyVar : string) =
    let letFun = LetFun(fName, pName, fBody, Var(lBodyVar))
    let substitutionEnv = [(lBodyVar, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun(fName, pName, fBody, subExpr), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunDoesNotSubstituteIntoLetBodyWhenFunctionIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) =
    let letFun = LetFun(fName, pName, fBody, Var(fName))
    let substitutionEnv = [(fName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(letFun, actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoLetFunSubstitutesIntoLetBodyWhenFunctionParameterIsInSubstitutionEnv
    (fName : string) (pName : string) (fBody : Expr) (subExpr : Expr) =
    let letFun = LetFun(fName, pName, fBody, Var(pName))
    let substitutionEnv = [(pName, subExpr)]

    let actual = substitute substitutionEnv letFun

    Assert.Equal(LetFun(fName, pName, fBody, subExpr), actual)

  [<Theory>]
  [<InlineAutoData("+")>]
  [<InlineAutoData("-")>]
  [<InlineAutoData("*")>]
  [<InlineAutoData("/")>]
  let SubstituteIntoOpSubstitutesIntoOperands op substitutionVar subExpr =
    let expr = Op(Var(substitutionVar), op, Var(substitutionVar))
    let env = (substitutionVar, subExpr)::[]

    let actual = substitute env expr

    Assert.Equal(Op(subExpr, op, subExpr), actual)

  [<Theory>]
  [<AutoData>]
  let SubstituteIntoCallSubstitutesIntoArgumentExpression 
    (fName : string) (argVar : string) (subExpr : Expr) =
    let call = Call(fName, Var(argVar))
    let env = (argVar, subExpr)::[]

    let actual = substitute env call

    Assert.Equal(Call(fName, subExpr), actual)


