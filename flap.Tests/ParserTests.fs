module ParserTests
  open System
  open Ast
  open Parser
  open Xunit
  open Xunit.Extensions
  open Ploeh.AutoFixture.Xunit
  open FParsec

  let parse = Parser.parse
  let runParser parser str = 
    runParserOnString parser () "Test input" str

  let isSuccess = function 
    | ParserResult.Success(e, _, _) -> true 
    | ParserResult.Failure(s, _, _) -> false

  let isExpression expected parseResult =
    match parseResult with
    | ParserResult.Success(actual, _, _) -> Assert.Equal(expected, actual)
    | ParserResult.Failure(str, _, _) as failure -> raise (Xunit.Sdk.AssertException ("Parse failure: " + str))

  [<Theory>]
  [<AutoData>]
  let ``integer parses integer`` (i : int) =
    let actual = runParser integer (i.ToString())

    actual |> isExpression (CstI(i))


  [<Theory>]
  [<InlineData("True", true)>]
  [<InlineData("False", false)>]
  let ``boolean parses True and False correctly`` input expected =
    let actual = runParser boolean input

    actual |> isExpression (CstB(expected))


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

    let actual = runParser stringLiteral literal

    actual |> isExpression (StringC(expected))


  [<Theory>]
  [<InlineData("myVar")>]
  [<InlineData("_myVar")>]
  [<InlineData("_myVar3")>]
  [<InlineData("_myVar_3")>]
  [<InlineData("var_3")>]
  let ``identifier parses identifier correctly`` input = 
    let actual = runParser Parser.identifier input

    actual |> isExpression input


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
  let operatorIsLeftAssociative op (x:Int32) (y:Int32) (z:Int32) = 
    let actual = parse (String.Format("{1}{0}{2}{0}{3}", op, x, y, z))

    Assert.Equal(Op(Op(CstI(x), op, CstI(y)), op, CstI(z)), actual)

  
  [<Theory>]
  [<AutoData>]
  let multiplicationHasHigherPrecedenceThanAddition 
    (x:Int32) (y:Int32) (z:Int32) = 
    let actual = parse (String.Format("{0}+{1}*{2}", x, y, z))

    Assert.Equal(Op(CstI(x), "+", Op(CstI(y), "*", CstI(z))), actual)

  [<Fact>]
  let ``ws0 accepts empty string`` () =
    let actual = runParser ws0 ""

    Assert.True(actual |> isSuccess)
  
  [<Fact>]
  let ``identifier parses identifier starting with _`` () =
    let actual = runParser Parser.identifier "_identifier"

    actual |> isExpression "_identifier"