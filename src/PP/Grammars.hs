{-|
Module      : PP.Grammars
Description : Common behavior for defined grammars
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Grammars
    ( InputGrammar(..)
    ) where

import           Data.Either
import qualified Text.Parsec as P

-- |Type class for grammars
class (Eq ast, Show ast, Read ast) => InputGrammar ast where
  -- |Entry parser
  parser :: P.Parsec String () ast
  -- |Parse to AST
  parse :: String -> Either P.ParseError ast
  parse = P.parse parser ""
