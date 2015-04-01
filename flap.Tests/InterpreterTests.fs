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
  [<InlineAutoData(10, "-", 15, -5)>]
  [<InlineAutoData(2,  "-", 1,   1)>]
  [<InlineAutoData(2,  "*", 1,   2)>]
  [<InlineAutoData(20, "*", 3,  60)>]
  [<InlineAutoData(6,  "/", 2,   3)>]
  [<InlineAutoData(7,  "/", 2,   3)>]
  let AdditionEvaluatedCorrectly i1 opSymb i2 expected = 
    let op = Op(CstI(i1), opSymb, CstI(i2))

    let actual = eval op []

    Assert.Equal(CstI(expected), actual)


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


