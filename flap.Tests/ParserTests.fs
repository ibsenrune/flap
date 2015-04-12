module ParserTests
  open Ast
  open Parser
  open Xunit
  open Xunit.Extensions
  open Ploeh.AutoFixture.Xunit

  [<Theory>]
  [<AutoData>]
  let parsesInteger (i : int) =
    let str = i.ToString()

    let actual = parse str

    Assert.Equal(CstI(i), actual)

  [<Fact>]
  let parsesTrueBoolean () =
    let str = "True"

    let actual = parse str

    Assert.Equal(CstB(true), actual)

  [<Fact>]
  let parsesFalseBoolean () =
    let str = "False"

    let actual = parse str

    Assert.Equal(CstB(false), actual)

  [<Theory>]
  [<InlineData("\"\"", "")>]
  [<InlineData("\" \"", " ")>]
  [<InlineData("\"\t\"", "\t")>]
  [<InlineData("\"\n\"", "\n")>]
  [<InlineData("\"\t\n\"", "\t\n")>]
  [<InlineData("\"\\r\\n\"", "\r\n")>]
  [<InlineData("\"abc\"", "abc")>]
  [<InlineData("\" abc \"", " abc ")>]
  [<InlineData("\" a\\nbc \"", " a\nbc ")>]
  let parsesStringLiterals literal expected =

    let actual = parse literal

    Assert.Equal(StringC(expected), actual)


  [<Theory>]
  [<InlineData("myVar")>]
  [<InlineData("_myVar")>]
  [<InlineData("_myVar3")>]
  [<InlineData("_myVar_3")>]
  [<InlineData("var_3")>]
  let parsesVar identifier = 
    let actual = parse identifier

    Assert.Equal(Var(identifier), actual)

  [<Theory>]
  [<InlineData("3+4", 3, "+", 4)>]
  [<InlineData("0+0", 0, "+", 0)>]
  [<InlineData("0+1", 0, "+", 1)>]
  [<InlineData("1+1", 1, "+", 1)>]
  [<InlineData("3/4", 3, "/", 4)>]
  [<InlineData("0/0", 0, "/", 0)>]
  [<InlineData("0/1", 0, "/", 1)>]
  [<InlineData("1/1", 1, "/", 1)>]
  [<InlineData("3*4", 3, "*", 4)>]
  [<InlineData("0*0", 0, "*", 0)>]
  [<InlineData("0*1", 0, "*", 1)>]
  [<InlineData("1*1", 1, "*", 1)>]
  [<InlineData("3-4", 3, "-", 4)>]
  [<InlineData("0-0", 0, "-", 0)>]
  [<InlineData("0-1", 0, "-", 1)>]
  [<InlineData("1-1", 1, "-", 1)>]
  let parsesBinaryOperation str lhs op rhs= 
    let actual = parse str

    Assert.Equal(Op(CstI(lhs), op, CstI(rhs)), actual)

  [<Theory>]
  [<InlineAutoData("+")>]
  [<InlineAutoData("-")>]
  [<InlineAutoData("*")>]
  [<InlineAutoData("/")>]
  let operatorIsLeftAssociative op (x:System.Int32) (y:System.Int32) (z:System.Int32) = 
    let actual = parse (System.String.Format("{1}{0}{2}{0}{3}", op, x, y, z))

    Assert.Equal(Op(Op(CstI(x), op, CstI(y)), op, CstI(z)), actual)

  
  [<Theory>]
  [<AutoData>]
  let multiplicationHasHigherPrecedenceThanAddition 
    (x:System.Int32) (y:System.Int32) (z:System.Int32) = 
    let actual = parse (System.String.Format("{0}+{1}*{2}", x, y, z))

    Assert.Equal(Op(CstI(x), "+", Op(CstI(y), "*", CstI(z))), actual)

  
