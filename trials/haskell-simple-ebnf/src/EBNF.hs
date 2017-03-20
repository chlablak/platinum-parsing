module EBNF
    ( TStatement(..)
    , statements
    ) where

import Text.Parsec as P

data TOperator =
    TConcatenation
  | TAlternation
    deriving (Show)

data TExpression =
    TTerminal String
  | TRule String
  | TBinary TExpression TOperator TExpression
    deriving (Show)

data TStatement =
    TDefinition TExpression TExpression
  | TStatements [TStatement]
      deriving (Show)

rule :: P.Parsec String st TExpression
rule = TRule <$> (P.try $ P.char '<' *> ruleDef <* P.char '>' <|> ruleDef)
  where
    ruleDef = (\a b -> a:b) <$> P.letter <*> (P.many alpha)
    alpha = P.letter <|> P.digit

operator :: P.Parsec String st TOperator
operator = choose <$> (P.oneOf ",|")
  where
    choose ',' = TConcatenation
    choose '|' = TAlternation

expression :: P.Parsec String st TExpression
expression = P.try binaryOp <|> part
  where
    binaryOp = TBinary <$> part <*> operator <*> expression
    part = TTerminal <$> (P.char '\'' *> (P.many1 $ P.noneOf "\'") <* P.char '\'')
      <|> rule

statements :: P.Parsec String st TStatement
statements = TStatements <$> P.many1 definition
  where
    definition = TDefinition <$> (rule <* P.char '=') <*> (expression <* P.char ';')
