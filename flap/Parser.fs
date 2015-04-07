module Parser
  open Ast
  open FParsec

  let parse s =
    
    let parseInteger = numberLiteral NumberLiteralOptions.DefaultInteger "integer"

    let result = runParserOnString parseInteger null "input" s

    match result with
    | ParserResult.Success(res, ustate, position) -> CstI(int res.String)
    | ParserResult.Failure(str, parseError, ustate) -> failwith (parseError.ToString())
