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
    ( -- * Canonical low-level rule
      Rule(..)
    , uniformize
    , extend
      -- * Canonical rules as Map
    , RuleSet
    , ruleSet
    , rule
      -- * Rules first set (Map)
    , FirstSet
    , firstSet
    , first
    ) where

import           Data.Either
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe

data Rule
  -- |A rule is defined by a non terminal and a list of Term and NonTerm
  -- The list should end with Empty
  = Rule String [Rule]
  -- |Non terminal string
  | NonTerm String
  -- |Terminal character
  | Term Char
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

-- |Rules as a map
type RuleSet = Map.Map String [[Rule]]

-- |Compute the rule set
ruleSet :: [Rule] -> RuleSet
ruleSet xs = Map.fromList [(n, collect n xs) | n <- names xs]
  where
    names = nub . map (\(Rule s _) -> s)
    collect n = map (\(Rule _ r) -> r) . filter (\(Rule s _) -> s == n)

-- |Get rule from a RuleSet
rule :: String -> RuleSet -> [Rule]
rule name rs = case Map.lookup name rs of
  Nothing -> []
  Just xs -> map (Rule name) xs

-- |First set type
type FirstSet = Map.Map String [Rule]

-- |Compute the complete first set
firstSet :: RuleSet -> FirstSet
firstSet rs = Map.mapWithKey (\k _ -> find k rs) rs
  where
    find name rs = nub . sort $ concatMap compute $ rule name rs
    compute (Rule _ [Empty]) = [Empty]
    compute (Rule name (x:xs)) = case compute x of
      [Empty] -> compute $ Rule name xs
      a       -> a
    compute a@(Term _) = [a]
    compute (NonTerm s) = find s rs
    compute Empty = [Empty]

-- |Compute first set of a given rule
first :: Rule -> FirstSet -> [Rule]
first Empty _           = [Empty]
first a@(Term _) _      = [a]
first (NonTerm s) fs    = fromMaybe [Empty] (Map.lookup s fs)
first (Rule _ (x:_)) fs = first x fs
