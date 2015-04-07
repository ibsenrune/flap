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
