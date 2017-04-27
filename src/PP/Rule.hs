{-|
Module      : PP.Rule
Description : Canonical rule representation
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Rule
    ( Rule(..)
    , uniformize
    ) where

import qualified Data.List as L

data Rule
  -- |A rule is defined by a non terminal and a list of Term and NonTerm
  -- The list should end with Empty
  = Rule String [Rule]
  -- |Terminal character
  | Term Char
  -- |Non terminal string
  | NonTerm String
  -- |Empty
  | Empty
  -- |Concatenated rules, useful for PP.InputGrammar.rules
  | Concat [Rule]
    deriving (Show, Eq, Read, Ord)

-- |Uniformize a list of rules
uniformize :: [Rule] -> [Rule]
uniformize = L.sort . L.nub . L.concatMap (flatten . clean)

-- |Clean a rule (remove Concat and useless Empty)
clean :: Rule -> Rule
clean (Rule s xs) = Rule s (cleaning xs)
  where
    cleaning []               = []
    cleaning a@[Empty]        = a
    cleaning (Empty : xs)     = cleaning xs -- useless Empty
    cleaning (Concat [] : xs) = cleaning xs
    cleaning (Concat xs : ys) = cleaning xs ++ cleaning ys -- remove Concat
    cleaning (Rule s xs : ys) = Rule s (cleaning xs) : cleaning ys -- inner Rule
    cleaning (x : xs)         = x : cleaning xs

-- |Replace and extract inner rules
flatten :: Rule -> [Rule]
flatten (Rule s xs) = Rule s (replace xs) : extract xs
  where
    replace []              = []
    replace (Rule s _ : xs) = NonTerm s : replace xs -- replacement
    replace (x : xs)        = x : replace xs
    extract []                  = []
    extract (r@(Rule _ _) : xs) = flatten r ++ extract xs -- extract inner Rule
    extract (x : xs)            = extract xs
