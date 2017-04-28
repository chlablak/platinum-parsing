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
    , extend
    ) where

import           Data.Either
import           Data.List

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
uniformize = sort . nub . concatMap (flatten . clean)

-- |Clean a rule (remove Concat and useless Empty)
clean :: Rule -> Rule
clean (Rule s xs) = Rule s (cleaning xs)
  where
    cleaning []               = []
    cleaning a@[Empty]        = a
    cleaning (Empty : xs)     = cleaning xs -- useless Empty
    cleaning (Concat [] : xs) = cleaning xs
    cleaning (Concat xs : ys) = cleaning (xs ++ ys) -- remove Concat
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

-- |Generate an augmented grammar
extend :: [Rule] -> Either String [Rule]
extend xs = case start xs of
  Left s  -> Left $ "cannot extend, " ++ s
  Right s -> Right $ Rule "__start" [NonTerm s, Empty] : xs

-- |Find start rule
start :: [Rule] -> Either String String
start xs = let c = candidates xs in
  case length c of
    1 -> Right $ head c
    _ -> Left $ "no start rule found (candidates: " ++ show c ++ ")"

-- |Find start rule candidates
candidates :: [Rule] -> [String]
candidates = map (fst . head) . filter (all snd) . grp . sortOn fst . evaluate
  where
    grp = groupBy (\(a, _) (b, _) -> a == b)
    evaluate []               = []
    evaluate (Rule s xs : ys) = (s, True) : evaluate xs ++ evaluate ys
    evaluate (NonTerm s : xs) = (s, False) : evaluate xs
    evaluate (_ : xs)         = evaluate xs
