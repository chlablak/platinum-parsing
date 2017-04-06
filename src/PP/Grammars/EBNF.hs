{-|
Module      : PP.Grammars.EBNF
Description : Defines a AST and parser for the EBNF language
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable

AST for the EBNF language.
Based on the grammar given in the ISO/IEC 14977:1996, page 10, part 8.2.
Comments are valid in EBNF, but are not present in this AST.
-}
module PP.Grammars.EBNF
    ( -- * Parser
      syntax
      -- * AST
    , Syntax(..)
    , SyntaxRule(..)
    , DefinitionsList(..)
    , SingleDefinition(..)
    , Term(..)
    , Exception(..)
    , Factor(..)
    , Primary(..)
    , MetaIdentifier(..)
    ) where

import           Control.Applicative                    ((<$>), (<*>))
import           Data.Maybe
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- |Start rule
newtype Syntax = Syntax [SyntaxRule]
  deriving (Show, Eq, Read)

-- |Defines the sequence of symbols represented by a MetaIdentifier
data SyntaxRule = SyntaxRule MetaIdentifier DefinitionsList
  deriving (Show, Eq, Read)

-- |Separates alternative SingleDefinition
newtype DefinitionsList = DefinitionsList [SingleDefinition]
  deriving (Show, Eq, Read)

-- |Separates successive Term
newtype SingleDefinition = SingleDefinition [Term]
  deriving (Show, Eq, Read)

-- |Represents any sequence of symbols that is defined by the Factor
-- but not defined by the Exception
data Term = Term Factor (Maybe Exception)
  deriving (Show, Eq, Read)

-- |A Factor may be used as an Exception if it could be replaced by a
-- Factor containing no MetaIdentifier
newtype Exception = Exception Factor
  deriving (Show, Eq, Read)

-- |The Integer specifies the number of repetitions of the Primay
data Factor = Factor (Maybe Integer) Primary
  deriving (Show, Eq, Read)

data Primary
  -- |Encloses symbols which are optional
  = OptionalSequence DefinitionsList
  -- |Encloses symbols which may be repeated any number of times
  | RepeatedSequence DefinitionsList
  -- |The meaning of a SpecialSequence is not defined in the standard metalangage
  | SpecialSequence String
  -- |Allows any DefinitionsList to be a Primary
  | GroupedSequence DefinitionsList
  -- |A Primary can be a MetaIdentifier
  | PrimaryMetaIdentifier MetaIdentifier
  -- |Represents the characters between the quote symbols '...' or "..."
  | TerminalString String
  -- |Empty Primary
  | Empty
    deriving (Show, Eq, Read)

-- |A MetaIdentifier is the name of a syntactic element of the langage being defined
newtype MetaIdentifier = MetaIdentifier String
  deriving (Show, Eq, Read)

-- |Lexer definitions for EBNF
lexer = Token.makeTokenParser def
  where
    def = emptyDef {
        Token.commentStart = "(*"
      , Token.commentEnd = "*)"
      , Token.commentLine = ""
      , Token.nestedComments = False
      , Token.identStart = letter
      , Token.identLetter = alphaNum <|> oneOf "_- "
      , Token.reservedNames = []
      , Token.reservedOpNames = ["=", ";", "|", ",", "-", "*"]
      , Token.caseSensitive = True
    }

identifier = Token.identifier lexer -- ^identifier
reservedOp = Token.reservedOp lexer -- ^reserved operator
natural = Token.natural lexer       -- ^natural positive number
whiteSpace = Token.whiteSpace lexer -- ^white spaces
parens = Token.parens lexer         -- ^between ( and )
braces = Token.braces lexer         -- ^between { and }
angles = Token.angles lexer         -- ^between < and >
brackets = Token.brackets lexer     -- ^between [ and ]

-- |EBNF start parser
syntax :: Parser Syntax
syntax = whiteSpace *> (Syntax <$> many1 syntaxRule)
  where
    syntaxRule = SyntaxRule <$> metaIdentifier <* reservedOp "="
                            <*> definitionsList <* reservedOp ";"
    definitionsList = DefinitionsList <$> sepBy1 singleDefinition
                                                 (reservedOp "|")
    singleDefinition = SingleDefinition <$> sepBy1 term (reservedOp ",")
    term = Term <$> factor <*> optionMaybe (reservedOp "-" *> exception)
    exception = Exception <$> factor
    factor = Factor <$> optionMaybe (natural <* reservedOp "*") <*> primary
    primary = option Empty (
              OptionalSequence <$> brackets definitionsList
          <|> RepeatedSequence <$> braces definitionsList
          <|> SpecialSequence <$> between (reservedOp "?")
                                          (reservedOp "?")
                                          (many anyChar)
          <|> GroupedSequence <$> parens definitionsList
          <|> PrimaryMetaIdentifier <$> metaIdentifier
          <|> TerminalString <$> ((char '"' *> many1 (noneOf "\"") <* char '"')
                             <|> (char '\'' *> many1 (noneOf "'") <* char '\''))
              ) -- end of option
    metaIdentifier = MetaIdentifier <$> (angles identifier <|> identifier)
