module Parser
  open Ast
  open FParsec

  let parse s =
    
    //Parses an integer
    let integer = (numberLiteral NumberLiteralOptions.DefaultInteger "integer") |>> fun number -> CstI(int number.String)


    //Parses a boolean value, i.e. "True" or "False"
    let boolean = (stringReturn "True"  (CstB(true)))
              <|> (stringReturn "False" (CstB(false)))

    let stringLiteral = 
      let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
      let unescape c = 
        match c with
        | 'n' -> "\n"
        | 'r' -> "\r"
        | 't' -> "\t"
        | c   -> string c
      let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
      let str = 
        between (pstring "\"") (pstring "\"")
              (stringsSepBy normalCharSnippet escapedChar)
      str |>> fun s -> StringC(s)

    let expr =
      integer <|>
      boolean <|>
      stringLiteral

    let result = runParserOnString expr null "input" s

    match result with
    | ParserResult.Success(expr, ustate, position) -> expr
    | ParserResult.Failure(str, parseError, ustate) -> failwith (parseError.ToString())
