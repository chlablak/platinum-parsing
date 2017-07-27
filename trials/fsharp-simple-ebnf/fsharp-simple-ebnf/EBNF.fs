module EBNF

open FParsec

// AST
// ---
type TOperator =
      TConcatenation
    | TAlternation

type TExpression =
      TTerminal of string
    | TRule of string
    | TBinary of TExpression * TOperator * TExpression

type TStatement =
      TDefinition of TExpression * TExpression
    | TStatements of TStatement list

// Parsers
// -------
let rule: Parser<TExpression, unit> =
    let alpha = letter <|> digit
    let ruleDef = many1Chars2 letter alpha
    ((pchar '<' >>. ruleDef .>> pchar '>') <|> ruleDef) |>> TRule

let operator: Parser<TOperator, unit> =
    let choose c =
        match c with
        | ',' -> TConcatenation
        | '|' -> TAlternation
    anyOf ",|" |>> choose

let expression, expressionRef = createParserForwardedToRef<TExpression, unit> ()

let statements: Parser<TStatement, unit> =
    let definition = pipe2 (rule .>> pchar '=') (expression .>> pchar ';') (fun a b -> TDefinition (a, b))
    many1 definition |>> TStatements

expressionRef :=
    let terminal = (pchar '\'' >>. many1Chars (noneOf "'") .>> pchar '\'') |>> TTerminal
    let part = terminal <|> rule
    let binaryOp = pipe3 part operator expression (fun a b c -> TBinary (a, b, c))
    attempt binaryOp <|> part
