{-|
Module      : PP.Grammars.EBNF
Description : Defines a AST for the EBNF language
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
    ( Syntax(..)
    , SyntaxRule(..)
    , DefinitionsList(..)
    , SingleDefinition(..)
    , Term(..)
    , Exception(..)
    , Factor(..)
    , Primary(..)
    , MetaIdentifier(..)
    ) where

import           Data.Maybe

-- |Start rule
newtype Syntax = Syntax [SyntaxRule]

-- |Defines the sequence of symbols represented by a MetaIdentifier
data SyntaxRule = SyntaxRule MetaIdentifier DefinitionsList

-- |Separates alternative SingleDefinition
newtype DefinitionsList = DefinitionsList [SingleDefinition]

-- |Separates successive Term
newtype SingleDefinition = SingleDefinition [Term]

-- |Represents any sequence of symbols that is defined by the Factor
-- but not defined by the Exception
data Term = Term Factor (Maybe Exception)

-- |A Factor may be used as an Exception if it could be replaced by a
-- Factor containing no MetaIdentifier
newtype Exception = Exception Factor

-- |The Integer specifies the number of repetitions of the Primay
data Factor = Factor (Maybe Integer) Primary

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

-- |A MetaIdentifier is the name of a syntactic element of the langage being defined
newtype MetaIdentifier = MetaIdentifier String
