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

  
