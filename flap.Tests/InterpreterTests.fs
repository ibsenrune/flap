module InterpreterTests

open Xunit.Extensions
open Ploeh.AutoFixture.Xunit
open Xunit
open Ast
open Interpreter


  [<Theory>]
  [<AutoData>]
  let CstIEvaluatedCorrectly expected = 
    let cst = CstI(expected)

    let actual = eval cst []
    
    Assert.Equal(expected, actual)

  [<Theory>]
  [<AutoData>]
  let VarEvaluatedCorrectly name expected = 
    let var = Var(name)
    let env = [(name,Int(expected))]

    let actual = eval var env
    
    Assert.Equal(expected, actual)

  [<Theory>]
  [<AutoData>]
  let LetEvaluatedCorrectly name i1 i2 =
    let lBody = CstI(i1)
    let env = [(name,i1)]
    let lExpr = Let(name,lBody,Op(Var(name),"+",CstI(i2)))

    let actual = eval lExpr []

    Assert.Equal(i1+i2, actual)

  [<Theory>]
  [<InlineAutoData(1,  "+", 2,   3)>]
  [<InlineAutoData(10, "+", 15, 25)>]
  [<InlineAutoData(10, "-", 15, -5)>]
  [<InlineAutoData(2,  "-", 1,   1)>]
  [<InlineAutoData(2,  "*", 1,   2)>]
  [<InlineAutoData(20, "*", 3,  60)>]
  [<InlineAutoData(6,  "/", 2,   3)>]
  [<InlineAutoData(7,  "/", 2,   3)>]
  let AdditionEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(expected, actual)
    

