module Parser
  open Ast
  open FParsec

  type private ExprParser = Parser<Expr,unit>

  let parse s =
    
    //Parses an integer
    let integer = (numberLiteral NumberLiteralOptions.DefaultInteger "integer") |>> fun number -> CstI(int number.String)

    //Parses a boolean value, i.e. "True" or "False"
    let boolean = (stringReturn "True"  (CstB(true)))
              <|> (stringReturn "False" (CstB(false)))
    
    //Parses a string literal, possibly containing escaped characters
    let stringLiteral = 
      //parses consecutive non-escaped characters
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
      str |>> StringC

    ///Parses an identifier
    let identifier : Parser<string, unit> = 
      let isIdentifierFirstChar c = isLetter c || c = '_'
      let isIdentifierChar c = isLetter c || isDigit c || c = '_'
      (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier")

    let var = identifier |>> Var

    let expr : ExprParser =
      integer <|>
      boolean <|>
      stringLiteral <|>
      var

    let result = runParserOnString expr () "input" s

    match result with
    | ParserResult.Success(expr, ustate, position) -> expr
    | ParserResult.Failure(str, parseError, ustate) -> failwith (parseError.ToString())
