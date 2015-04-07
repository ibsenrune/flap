module Parser
  open Ast
  open FParsec

  let parse s =
    
    //Parses an integer
    let integer = (numberLiteral NumberLiteralOptions.DefaultInteger "integer") |>> fun number -> CstI(int number.String)


    //Parses a boolean value, i.e. "True" or "False"
    let boolean = (stringReturn "True"  (CstB(true)))
              <|> (stringReturn "False" (CstB(false)))

    let expr =
      integer <|>
      boolean

    let result = runParserOnString expr null "input" s

    match result with
    | ParserResult.Success(expr, ustate, position) -> expr
    | ParserResult.Failure(str, parseError, ustate) -> failwith (parseError.ToString())
