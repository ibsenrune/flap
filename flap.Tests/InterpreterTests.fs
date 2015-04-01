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
    
    Assert.Equal(cst, actual)

  [<Theory>]
  [<AutoData>]
  let VarEvaluatedCorrectly name expected = 
    let var = Var(name)
    let env = [(name,ExprValue(CstI(expected), []))]

    let actual = eval var env
    
    Assert.Equal(CstI(expected), actual)

  
  [<Theory>]
  [<AutoData>]
  let VarThrowsIfVariableRefersToClosure fName pName fBody =
    let closure = Closure(fName, pName, fBody, [])
    let env = (fName, closure)::[]
    let varExpr = Var(fName)

    Assert.Throws<System.Exception>(fun () -> eval varExpr env |> ignore)


  [<Theory>]
  [<AutoData>]
  let LetEvaluatedCorrectly name i1 i2 =
    let lBody = CstI(i1)
    let env = [(name,i1)]
    let lExpr = Let(name,lBody,Op(Var(name),"+",CstI(i2)))

    let actual = eval lExpr []

    Assert.Equal(CstI(i1+i2), actual)


  [<Theory>]
  [<InlineAutoData(1,  "+", 2,   3)>]
  [<InlineAutoData(10, "+", 15, 25)>]
  [<InlineAutoData(6,  "/", 2,   3)>]
  [<InlineAutoData(7,  "/", 2,   3)>]
  let AdditionEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(CstI(expected), actual)


  [<Theory>]
  [<InlineAutoData(10, "-", 15, -5)>]
  [<InlineAutoData(2,  "-", 1,   1)>]
  let SubtractionEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(CstI(expected), actual)


  [<Theory>]
  [<InlineAutoData(2,  "*", 1,   2)>]
  [<InlineAutoData(20, "*", 3,  60)>]
  let MultiplicationEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(CstI(expected), actual)  

  [<Theory>]
  [<InlineAutoData(6,  "/", 2,   3)>]
  [<InlineAutoData(7,  "/", 2,   3)>]
  let DivisionEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(CstI(expected), actual)


  [<Theory>]
  [<InlineAutoData("+")>]
  [<InlineAutoData("-")>]
  [<InlineAutoData("*")>]
  [<InlineAutoData("/")>]
  let ApplyingMathOperatorsToNonIntegersThrows opSymb b i = 
    let env = []

    Assert.Throws<System.Exception>(fun () -> 
      (eval (Op(CstB(b), opSymb, CstI(i)))  env) |> ignore) |> ignore;

    Assert.Throws<System.Exception>(fun () -> 
      (eval (Op(CstI(i), opSymb, CstB(b)))  env) |> ignore) |> ignore;

    Assert.Throws<System.Exception>(fun () -> 
      (eval (Op(CstB(b), opSymb, CstB(b)))  env) |> ignore) |> ignore

  [<Theory>]
  [<InlineAutoData(-1, -1, true)>]
  [<InlineAutoData(501, 501, true)>]
  [<InlineAutoData(-1, 1, false)>]
  [<InlineAutoData(1, -1, false)>]
  [<InlineAutoData(39, 309, false)>]
  let EqualityBetweenIntegersEvaluatedCorrectly i1 i2 expected =
    let expr = Op(CstI(i1), "=", CstI(i2))

    let actual = eval expr []

    Assert.Equal(CstB(expected), actual)


  [<Theory>]
  [<InlineAutoData(true, true, true)>]
  [<InlineAutoData(true, false, false)>]
  [<InlineAutoData(false, true, false)>]
  [<InlineAutoData(false, false, true)>]
  let EqualityBetweenBooleansEvaluatedCorrectly b1 b2 expected =
    let expr = Op(CstB(b1), "=", CstB(b2))

    let actual = eval expr []

    Assert.Equal(CstB(expected), actual)


  [<Theory>]
  [<AutoData>]
  let EvaluatingEqualityBetweenNonCompatibleTypesThrows b i =
    let env = []
    
    Assert.Throws<System.Exception>(fun () -> 
      (eval (Op(CstB(b), "=", CstI(i)))  env) |> ignore) |> ignore;

    Assert.Throws<System.Exception>(fun () -> 
      (eval (Op(CstI(i), "=", CstB(b)))  env) |> ignore) |> ignore;
   

  [<Theory>]
  [<AutoData>]
  let CanEvaluateLetFunWithFunctionCall fName pName fBody i =
    let letFun = LetFun(fName, pName, fBody, CstI(i))

    let actual = eval letFun []

    Assert.Equal(CstI(i), actual)

  [<Theory>]
  [<AutoData>]
  let CallEvaluatedCorrectly () =
    let closure = Closure("f", "x", Op(Var("x"), "*", CstI(2)), [])
    let env = ("f", closure)::[]
    let call = Call("f", CstI(3))

    let actual = eval call env

    Assert.Equal(CstI(3*2), actual)


  [<Theory>]
  [<InlineAutoData(true, 2, 3, 2)>]
  [<InlineAutoData(false, 2, 3, 3)>]
  let IfEvaluatedCorrectly b tValue fValue expectedValue =
    let ifExpr = If(CstB(b), CstI(tValue), CstI(fValue))

    let actual = eval ifExpr []

    Assert.Equal(CstI(expectedValue), actual)

  
  [<Theory>]
  [<AutoData>]
  let EvaluatingIfThrowsIfConditionalExpressionIsNotBoolean i tExpr fExpr =
    let ifExpr = If(CstI(i), tExpr, fExpr)

    Assert.Throws<System.Exception>(fun () -> eval ifExpr [] |> ignore)



