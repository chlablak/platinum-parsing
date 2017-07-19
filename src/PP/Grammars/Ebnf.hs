{-|
Module      : PP.Grammars.Ebnf
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
module PP.Grammars.Ebnf
    ( -- * AST
      Syntax(..)
      -- ** Inner ASTs
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
import qualified Data.List                              as L
import           Data.Maybe
import           Data.Text                              (pack, strip, unpack)
import           PP.Grammar
import           PP.Grammars.LexicalHelper              (LexicalRule,
                                                         lexicalString)
import qualified PP.Rule                                as R
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token    as Token

-- |Start rule
newtype Syntax = Syntax [SyntaxRule]
  deriving (Show, Eq)

-- |Syntax rule
data SyntaxRule
  -- |Defines the sequence of symbols represented by a MetaIdentifier
  = SyntaxRule MetaIdentifier DefinitionsList
  -- |Defines a lexical definition inside the EBNF grammar
  | LexicalInner LexicalRule
    deriving (Show, Eq)

-- |Separates alternative SingleDefinition
newtype DefinitionsList = DefinitionsList [SingleDefinition]
  deriving (Show, Eq)

-- |Separates successive Term
newtype SingleDefinition = SingleDefinition [Term]
  deriving (Show, Eq)

-- |Represents any sequence of symbols that is defined by the Factor
-- but not defined by the Exception
data Term = Term Factor (Maybe Exception)
  deriving (Show, Eq)

-- |A Factor may be used as an Exception if it could be replaced by a
-- Factor containing no MetaIdentifier
newtype Exception = Exception Factor
  deriving (Show, Eq)

-- |The Integer specifies the number of repetitions of the Primay
data Factor = Factor (Maybe Integer) Primary
  deriving (Show, Eq)

-- |Primary
data Primary
  -- |Encloses symbols which are optional
  = OptionalSequence DefinitionsList
  -- |Encloses symbols which may be repeated any number of times
  | RepeatedSequence DefinitionsList
  -- |Allows any DefinitionsList to be a Primary
  | GroupedSequence DefinitionsList
  -- |A Primary can be a MetaIdentifier
  | PrimaryMetaIdentifier MetaIdentifier
  -- |Represents the characters between the quote symbols '...' or "..."
  | TerminalString String
  -- |Empty Primary
  | Empty
    deriving (Show, Eq)

-- |A MetaIdentifier is the name of a syntactic element of the langage being defined
newtype MetaIdentifier = MetaIdentifier String
  deriving (Show, Eq)

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

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
stringLiteral = Token.stringLiteral lexer
natural = Token.natural lexer
whiteSpace = Token.whiteSpace lexer
parens = Token.parens lexer
braces = Token.braces lexer
angles = Token.angles lexer
brackets = Token.brackets lexer

-- |Syntax parser
syntax :: Parser Syntax
syntax = whiteSpace *> (Syntax <$> many1 syntaxRule) <?> "syntax"

-- |SyntaxRule parser
syntaxRule :: Parser SyntaxRule
syntaxRule = try (SyntaxRule <$> (metaIdentifier <* reservedOp "=")
                             <*> (definitionsList <* reservedOp ";"))
          <|> LexicalInner <$> parser
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
      <|> GroupedSequence <$> parens definitionsList
      <|> PrimaryMetaIdentifier <$> metaIdentifier
      <|> TerminalString <$> stringLiteral
          ) -- end of option
  <?> "primary"

-- |MetaIdentifier parser
metaIdentifier :: Parser MetaIdentifier
metaIdentifier = trimMetaIdentifier <$> (angles identifier <|> identifier)
  <?> "meta identifier"
  where
    trimMetaIdentifier = MetaIdentifier . unpack . strip . pack

-- |Lexify an EBNF syntax tree
lexifySyntax :: Syntax -> Syntax
lexifySyntax s = replaceTerm tok $ addLexicalInner tok s
  where
    tok = generateTokens $ findTerm s
    findTerm (Syntax srs) = L.concatMap findTerm' srs
    findTerm' (SyntaxRule _ dl) = findTerm'' dl
    findTerm' (LexicalInner _)  = []
    findTerm'' (DefinitionsList sds) = L.concatMap findTerm''' sds
    findTerm''' (SingleDefinition ts) = L.concatMap findTerm'''' ts
    findTerm'''' (Term f _) = findTerm''''' f
    findTerm''''' (Factor _ p) = findTerm'''''' p
    findTerm'''''' (OptionalSequence dl)      = findTerm'' dl
    findTerm'''''' (RepeatedSequence dl)      = findTerm'' dl
    findTerm'''''' (GroupedSequence dl)       = findTerm'' dl
    findTerm'''''' (PrimaryMetaIdentifier mi) = []
    findTerm'''''' (TerminalString term)      = [term]
    findTerm'''''' Empty                      = []
    generateTokens = map (\t -> (t, "__token_" ++ t)) . L.nub
    addLexicalInner [] s = s
    addLexicalInner ((n, t):ts) (Syntax srs) =
      addLexicalInner ts $ Syntax $ LexicalInner (lexicalString t n) : srs
    replaceTerm [] s                = s
    replaceTerm (t:ts) (Syntax srs) =
      replaceTerm ts $ Syntax $ L.map (replaceTerm' t) srs
    replaceTerm' t (SyntaxRule r dl)   = SyntaxRule r $ replaceTerm'' t dl
    replaceTerm' _ li@(LexicalInner _) = li
    replaceTerm'' t (DefinitionsList sds) =
      DefinitionsList $ L.map (replaceTerm''' t) sds
    replaceTerm''' t (SingleDefinition ts) =
      SingleDefinition $ L.map (replaceTerm'''' t) ts
    replaceTerm'''' t (Term f e) = Term (replaceTerm''''' t f) e
    replaceTerm''''' t (Factor f p) = Factor f $ replaceTerm'''''' t p
    replaceTerm'''''' t (OptionalSequence dl)         =
      OptionalSequence $ replaceTerm'' t dl
    replaceTerm'''''' t (RepeatedSequence dl)         =
      RepeatedSequence $ replaceTerm'' t dl
    replaceTerm'''''' t (GroupedSequence dl)          =
      GroupedSequence $ replaceTerm'' t dl
    replaceTerm'''''' (n, t) ts@(TerminalString s)         =
      if n == s then PrimaryMetaIdentifier (MetaIdentifier t) else ts
    replaceTerm'''''' _ p                         = p

-- * InputGrammar instances for EBNF AST
instance InputGrammar Syntax where
  parser = syntax
  stringify (Syntax [])     = ""
  stringify (Syntax [sr])   = stringify sr
  stringify (Syntax (sr:r)) = stringify sr ++ "\n" ++ stringify (Syntax r)
  rules (Syntax srs) = R.uniformize $ L.concatMap rules srs
  lexify = lexifySyntax

instance InputGrammar SyntaxRule where
  parser = syntaxRule
  stringify (SyntaxRule mi dl) = stringify mi ++ "=" ++ stringify dl ++ ";"
  stringify (LexicalInner lr)  = stringify lr
  rules (SyntaxRule (MetaIdentifier mi) dl) =
    [R.Rule mi [r, R.Empty] | r <- rules dl]
  rules (LexicalInner lr) = rules lr

instance InputGrammar DefinitionsList where
  parser = definitionsList
  stringify (DefinitionsList []) = ""
  stringify (DefinitionsList [sd]) = stringify sd
  stringify (DefinitionsList (sd:r)) =
    stringify sd ++ "|" ++ stringify (DefinitionsList r)
  rules (DefinitionsList sds) = L.concatMap rules sds

instance InputGrammar SingleDefinition where
  parser = singleDefinition
  stringify (SingleDefinition []) = ""
  stringify (SingleDefinition [t]) = stringify t
  stringify (SingleDefinition (t:r)) =
    stringify t ++ "," ++ stringify (SingleDefinition r)
  rules (SingleDefinition [t]) = rules t
  rules (SingleDefinition (t:ts)) =
    [R.Concat [r,n] | r <- rules t, n <- rules (SingleDefinition ts)]

instance InputGrammar Term where
  parser = term
  stringify (Term f Nothing)  = stringify f
  stringify (Term f (Just e)) = stringify f ++ "-" ++ stringify e
  rules (Term f Nothing) = rules f
  rules _                = error "no translation for exception" -- ... yet

instance InputGrammar Exception where
  parser = exception
  stringify (Exception f) = stringify f
  rules _ = undefined -- should not be called, look at the Term instance

instance InputGrammar Factor where
  parser = factor
  stringify (Factor Nothing p)  = stringify p
  stringify (Factor (Just i) p) = show i ++ "*" ++ stringify p
  rules (Factor Nothing p)  = rules p
  rules (Factor (Just i) p) =
    [R.Concat . concat $ replicate (fromIntegral i) (rules p)]

instance InputGrammar Primary where
  parser = primary
  stringify (OptionalSequence dl)      = "[" ++ stringify dl ++ "]"
  stringify (RepeatedSequence dl)      = "{" ++ stringify dl ++ "}"
  stringify (GroupedSequence dl)       = "(" ++ stringify dl ++ ")"
  stringify (PrimaryMetaIdentifier mi) = stringify mi
  stringify (TerminalString s)         = show s
  stringify Empty                      = ""
  rules a@(OptionalSequence dl)    = let x = stringify a in
    R.NonTerm x : R.Rule x [R.Empty] : [R.Rule x [r, R.Empty] | r <- rules dl]
  rules a@(RepeatedSequence dl)    = let x = stringify a in
    R.NonTerm x : R.Rule x [R.Empty] :
      [R.Rule x [r, R.NonTerm x, R.Empty] | r <- rules dl]
  rules a@(GroupedSequence dl)     = let x = stringify a in
    R.NonTerm x : [R.Rule x [r, R.Empty] | r <- rules dl]
  rules (PrimaryMetaIdentifier mi) = rules mi
  rules (TerminalString s)         = [R.Concat $ L.map R.Term s]
  rules Empty                      = [R.Empty]

instance InputGrammar MetaIdentifier where
  parser = metaIdentifier
  stringify (MetaIdentifier s) = "<" ++ s ++ ">"
  rules (MetaIdentifier s) = [R.NonTerm s]
