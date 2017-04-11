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
    ( -- * AST
      Syntax(..)
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
import           Data.Text                              (pack, strip, unpack)
import           PP.Grammar
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

-- |Syntax parser
syntax :: Parser Syntax
syntax = whiteSpace *> (Syntax <$> many1 syntaxRule) <?> "syntax"

-- |SyntaxRule parser
syntaxRule :: Parser SyntaxRule
syntaxRule = SyntaxRule <$> metaIdentifier <* reservedOp "="
                        <*> definitionsList <* reservedOp ";"
  <?> "syntax rule"

-- |DefinitionsList parser
definitionsList :: Parser DefinitionsList
definitionsList = DefinitionsList <$> sepBy1 singleDefinition (reservedOp "|")
  <?> "definitions list"

-- |SingleDefinition parser
singleDefinition :: Parser SingleDefinition
singleDefinition = SingleDefinition <$> sepBy1 term (reservedOp ",")
  <?> "single definition"

-- |Term parser
term :: Parser Term
term = Term <$> factor <*> optionMaybe (reservedOp "-" *> exception)
  <?> "term"

-- |Exception parser
exception :: Parser Exception
exception = Exception <$> factor <?> "exception"

-- |Factor parser
factor :: Parser Factor
factor = Factor <$> optionMaybe (natural <* reservedOp "*") <*> primary
  <?> "factor"

-- |Primary parser
primary :: Parser Primary
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
  <?> "primary"

-- |MetaIdentifier parser
metaIdentifier :: Parser MetaIdentifier
metaIdentifier = trimMetaIdentifier <$> (angles identifier <|> identifier)
  <?> "meta identifier"
  where
    trimMetaIdentifier = MetaIdentifier . unpack . strip . pack

-- * InputGrammar instances for EBNF AST
instance InputGrammar Syntax where
  parser = syntax
  stringify (Syntax [])     = ""
  stringify (Syntax [sr])   = stringify sr
  stringify (Syntax (sr:r)) = stringify sr ++ "\n" ++ stringify (Syntax r)

instance InputGrammar SyntaxRule where
  parser = syntaxRule
  stringify (SyntaxRule mi dl) = stringify mi ++ "=" ++ stringify dl ++ ";"

instance InputGrammar DefinitionsList where
  parser = definitionsList
  stringify (DefinitionsList []) = ""
  stringify (DefinitionsList [sd]) = stringify sd
  stringify (DefinitionsList (sd:r)) =
    stringify sd ++ "|" ++ stringify (DefinitionsList r)

instance InputGrammar SingleDefinition where
  parser = singleDefinition
  stringify (SingleDefinition []) = ""
  stringify (SingleDefinition [t]) = stringify t
  stringify (SingleDefinition (t:r)) =
    stringify t ++ "," ++ stringify (SingleDefinition r)

instance InputGrammar Term where
  parser = term
  stringify (Term f Nothing)  = stringify f
  stringify (Term f (Just e)) = stringify f ++ "-" ++ stringify e

instance InputGrammar Exception where
  parser = exception
  stringify (Exception f) = stringify f

instance InputGrammar Factor where
  parser = factor
  stringify (Factor Nothing p)  = stringify p
  stringify (Factor (Just i) p) = show i ++ "*" ++ stringify p

instance InputGrammar Primary where
  parser = primary
  stringify (OptionalSequence dl)      = "[" ++ stringify dl ++ "]"
  stringify (RepeatedSequence dl)      = "{" ++ stringify dl ++ "}"
  stringify (SpecialSequence s)        = "?" ++ s ++ "?"
  stringify (GroupedSequence dl)       = "(" ++ stringify dl ++ ")"
  stringify (PrimaryMetaIdentifier mi) = stringify mi
  stringify (TerminalString s)         = show s
  stringify Empty                      = ""

instance InputGrammar MetaIdentifier where
  parser = metaIdentifier
  stringify (MetaIdentifier s) = "<" ++ s ++ ">"
