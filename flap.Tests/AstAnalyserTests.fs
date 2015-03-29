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

    let actual = substitute p expr cst

    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsNotSubstitutionVarDoesNothing 
        (p : string) (var : string) (expr : Expr) =
    let varExpr = Var(var)

    let actual = substitute p expr varExpr

    Assert.NotEqual(p, var);
    Assert.Equal(varExpr, actual)

  [<Theory>]
  [<AutoData>]
  let SubstitutingIntoVarWhenItIsSubstitutionVarResultsInSubstitedExpression
        (p : string) (expr : Expr) =
    let varExpr = Var(p)

    let actual = substitute p expr varExpr

    Assert.Equal(expr, actual)


