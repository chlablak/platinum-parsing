{-|
Module      : PP.Grammars.LexicalHelper
Description : Add lexical support to other grammars
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Grammars.LexicalHelper
    ( -- *AST helper for other grammars
      LexicalRule(..)
    , LexicalDefinitionList(..)
    , LexicalDefinition(..)
      -- *Helpers
    , lexicalString
    ) where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Text                              (pack, strip, unpack)
import           PP.Grammar
import qualified PP.Rule                                as R
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- |Defines a lexical rule represented by a identifier
data LexicalRule = LexicalRule String LexicalDefinitionList
  deriving (Show, Eq)

-- |Defines the definition list for a lexical rule
newtype LexicalDefinitionList = LexicalDefinitionList [LexicalDefinition]
  deriving (Show, Eq)

data LexicalDefinition
  -- |Regular expression as a terminal string
  = LexicalString String
  -- |Other lexical rule identifier
  | LexicalIdentifier String
    deriving (Show, Eq)

-- |Construct a simple lexical rule
lexicalString :: String -> String -> LexicalRule
lexicalString n s = LexicalRule n $ LexicalDefinitionList [LexicalString s]

-- |Parsing helpers
lexer = Token.makeTokenParser def
  where
    def = emptyDef {
        Token.commentStart = ""
      , Token.commentEnd = ""
      , Token.commentLine = ""
      , Token.nestedComments = False
      , Token.identStart = letter
      , Token.identLetter = alphaNum <|> oneOf "_- "
      , Token.reservedNames = []
      , Token.reservedOpNames = ["%=", ";", ","]
      , Token.caseSensitive = True
    }
identifier = Token.identifier lexer       -- ^identifier
reservedOp = Token.reservedOp lexer       -- ^reserved operator
stringLiteral = Token.stringLiteral lexer -- ^string literal
whiteSpace = Token.whiteSpace lexer       -- ^white spaces

-- |Parser for LexicalRule
lexicalRule :: Parser LexicalRule
lexicalRule = whiteSpace *>
              (LexicalRule <$> (lexicalIdentifier <* reservedOp "%=")
                           <*> (lexicalDefinitionList <* reservedOp ";"))
  <?> "lexical rule"

-- |Parser for LexicalDefinitionList$
lexicalDefinitionList :: Parser LexicalDefinitionList
lexicalDefinitionList = LexicalDefinitionList <$> sepBy1 lexicalDefinition (reservedOp ",")
  <?> "lexical definition list"

-- |Parser for LexicalDefinition
lexicalDefinition :: Parser LexicalDefinition
lexicalDefinition = LexicalString <$> stringLiteral
                <|> LexicalIdentifier <$> lexicalIdentifier
  <?> "lexical definition"

-- |Parser for LexicalIdentifier, helper
lexicalIdentifier :: Parser String
lexicalIdentifier = (unpack . strip . pack) <$> identifier
  <?> "lexical identifier"

-- *Associated InputGrammar instances
instance InputGrammar LexicalRule where
  parser = lexicalRule
  stringify (LexicalRule li xs) = li ++ "%=" ++ stringify xs ++ ";"
  rules (LexicalRule li xs) = R.uniformize [R.Rule li (rules xs ++ [R.Empty])]

instance InputGrammar LexicalDefinitionList where
  parser = lexicalDefinitionList
  stringify (LexicalDefinitionList []) = ""
  stringify (LexicalDefinitionList [x]) = stringify x
  stringify (LexicalDefinitionList (x:xs)) =
    stringify x ++ "," ++ stringify (LexicalDefinitionList xs)
  rules (LexicalDefinitionList xs) = [head (rules x) | x <- xs]

instance InputGrammar LexicalDefinition where
  parser = lexicalDefinition
  stringify (LexicalString x)     = show x
  stringify (LexicalIdentifier x) = x
  rules (LexicalString x)     = [R.RegEx x]
  rules (LexicalIdentifier x) = [R.NonTerm x]
