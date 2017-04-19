{-|
Module      : PP.Grammar
Description : Common behavior for defined grammars
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Grammar
    ( To
    , InputGrammar(..)
    ) where

import           Data.Either
import qualified Text.Parsec as P

-- |Syntactic sugar
-- For exemple: `case PP.parse input :: (PP.To Ebnf.Syntax) of ...`
type To ast = Either P.ParseError ast

-- |Type class for grammars
class (Eq ast, Show ast, Read ast) => InputGrammar ast where
  -- |Entry parser
  parser :: P.Parsec String () ast
  -- |Parse String to AST
  parse :: String -> To ast
  parse = P.parse parser ""
  -- |AST to String
  stringify :: ast -> String
  stringify = show
