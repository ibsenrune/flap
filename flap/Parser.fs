module Parser
  open Ast
  open FParsec
  open FParsec.Primitives

  type private ExprParser = Parser<Expr,unit>

  let parse s =
    
    let (expression : Parser<Expr,_>, expressionRef : Parser<Expr,_> ref) = createParserForwardedToRef() //http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html

    // white space or comment
    let ws = (many spaces1 |>> ignore) <?> "whitespace"

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

    let letInEnd = 
      let letParser = pstring "let" >>. ws >>. identifier .>> ws .>> pstring "=" .>> ws
      let inParser = ws >>. pstring "in" .>> ws
      let endParser = ws >>. pstring "end"
      pipe5 letParser expression inParser expression endParser 
        (fun p letExpr _ letBody _ -> Let(p, letExpr, letBody))

    let parenExpr = pstring "(" >>. expression .>> pstring ")"

    let aExpr = 
      integer <|>
      boolean <|>
      stringLiteral <|>
      var <|>
      parenExpr

    // The operator parser from FParsec takes care of precedence issues
    let opp = new OperatorPrecedenceParser<_,_,_>()

    let makeOperators =
      let precedence = [
        ["*"; "/"], Associativity.Left
        ["+"; "-"], Associativity.Left
      ]
      // we start with operators with highest priority, then we decrement the counter.
      let precCounter = ref 20 //(we have at most 20 different priorities)
      let addInfix li =
        for ops, assoc in li do
          decr precCounter
          for op in ops do
            opp.AddOperator(InfixOperator(op, ws, !precCounter, assoc, fun x y -> Op(x, op, y)))
      opp.TermParser <- aExpr
      addInfix precedence

    let exprParser : ExprParser =
      (attempt opp.ExpressionParser) <|>
      (attempt letInEnd) <|>
      aExpr
      
    

    do expressionRef := exprParser //http://hestia.typepad.com/flatlander/2011/07/recursive-parsers-in-fparsec.html

    let result = runParserOnString exprParser () "input" s

    match result with
    | ParserResult.Success(expr, ustate, position) -> expr
    | ParserResult.Failure(str, parseError, ustate) -> failwith (parseError.ToString())
