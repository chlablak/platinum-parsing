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
    , rules'
    ) where

import           Control.Exception
import           Data.Either
import           PP.Rule           (Rule)
import qualified Text.Parsec       as P

-- |Syntactic sugar
-- For exemple: `case PP.parseAst input :: (PP.To Ebnf.Syntax) of ...`
type To ast = Either P.ParseError ast

-- |Type class for grammars
class (Eq ast, Show ast) => InputGrammar ast where
  -- |Entry parser
  parser :: P.Parsec String () ast
  -- |Parse String to AST
  parseAst :: String -> To ast
  parseAst = P.parse parser ""
  -- |AST to String
  stringify :: ast -> String
  stringify = show
  -- |AST to canonical rules
  rules :: ast -> [Rule]
  -- |Transform terminals to lexical rules
  lexify :: ast -> ast
  lexify = id

-- |Exception-safe version of `rules`
rules' :: (InputGrammar ast) => ast -> IO (Either String [Rule])
rules' ast = do
    a <- try (evaluate $ rules ast) :: IO (Either SomeException [Rule])
    case a of
        Left e  -> return $ Left $ head $ lines $ displayException e
        Right r -> return $ Right r
