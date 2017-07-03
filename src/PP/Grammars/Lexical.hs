{-|
Module      : PP.Grammars.Lexical
Description : Defines an AST and parser for a lexical grammar
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Grammars.Lexical
    ( -- *AST
      RegExpr(..)
    ) where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Text                              (pack, strip, unpack)
import           PP.Grammar
import qualified PP.Rule                                as R
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- |Lexical rule AST
data RegExpr
  = RegExpr [RegExpr]       -- ^Composed of many choices
  | Choice [RegExpr]        -- ^Composed of many expressions
  | Many0 RegExpr           -- ^`a` many times
  | Many1 RegExpr           -- ^`a` many times, without 0
  | Option RegExpr          -- ^`a` 0 or 1 time
  | Group RegExpr           -- ^`a` grouped (parenthesis)
  | Class [RegExpr]         -- ^One character in the sub-classes
  | Interval Char Char      -- ^One character in the interval
  | Value Char              -- ^One specific character
  | Any                     -- ^One character
    deriving (Show, Eq)

-- |Lexical expression parser (input is reversed)
regExprP :: Parser RegExpr
regExprP = RegExpr . reverse <$> sepBy1 choiceP (char '|')
  where
    choiceP = Choice . reverse <$> many exprP
    exprP = try groupP
        <|> try classP
        <|> try many0P
        <|> try many1P
        <|> try optionP
        <|> try anyP
        <|>     valueP
    many0P = Many0 <$> (char '*' *> exprP)
    many1P = Many1 <$> (char '+' *> exprP)
    optionP = Option <$> (char '?' *> exprP)
    groupP = Group <$> between (char ')') (char '(') regExprP
    classP = Class . reverse <$> between (char ']') (char '[')
                                         (many1 (try intervalP <|> classValueP))
    intervalP = flip Interval <$> (anyChar <* char '-') <*> anyChar
    valueP = Value <$> noneOf "|*+?()[]"
    classValueP = Value <$> noneOf "["
    anyP = Any <$ char '.'

-- |RegExpr InputGrammar instance
instance InputGrammar RegExpr where
  parser = regExprP
  parseAst = parse regExprP "" . reverse
  stringify (RegExpr [])     = ""
  stringify (RegExpr [x])    = stringify x
  stringify (RegExpr (x:xs)) = stringify x ++ "|" ++ stringify (RegExpr xs)
  stringify (Choice xs)      = concatMap stringify xs
  stringify (Many0 a)        = stringify a ++ "*"
  stringify (Many1 a)        = stringify a ++ "+"
  stringify (Option a)       = stringify a ++ "?"
  stringify (Group a)        = "(" ++ stringify a ++ ")"
  stringify (Class xs)       = "[" ++ concatMap stringify xs ++ "]"
  stringify (Interval i j)   = [i,'-',j]
  stringify (Value i)        = [i]
  stringify Any              = "."
  rules r = [R.RegEx $ stringify r]
