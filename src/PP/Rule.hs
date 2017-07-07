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
    , separate
    , regexfy
      -- * Canonical rules as Map
    , RuleSet
    , ruleSet
    , rule
    , check
      -- * Rules first set (Map)
    , FirstSet
    , firstSet
    , first
    ) where

import           Data.Either
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           PP.Lexer        (IToken)

data Rule
  -- |A rule is defined by a non terminal and a list of Term and NonTerm
  -- The list should end with Empty
  = Rule String [Rule]
  -- |Non terminal string
  | NonTerm String
  -- |Terminal character
  | Term IToken
  -- |Terminal token
  | TermToken String
  -- |Empty
  | Empty
  -- |Concatenated rules, useful for PP.InputGrammar.rules
  | Concat [Rule]
  -- |Regular expression, useful for lexical rules
  | RegEx String
  | RegExString String -- ^No parse-string
    deriving (Eq, Ord)

instance Show Rule where
  show (Rule a xs) = a ++ " -> " ++ right xs
    where
      right []     = ""
      right [x]    = show x
      right (x:xs) = show x ++ "," ++ right xs
  show (NonTerm a) = a
  show (Term c) = show c
  show (TermToken t) = '%' : t
  show Empty = "$"
  show (Concat xs) = "Concat " ++ show xs
  show (RegEx re) = '%' : show re
  show (RegExString s) = show s

-- |Uniformize a list of rules
-- `uniformize = sort . nub . concatMap (flatten . clean)`
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

-- |Check a rule set, return: (errors, warnings)
check :: RuleSet -> ([String], [String])
check rs = (missing ++ leftRec, unused)
  where
    missing = ["missing non-terminal: " ++ n | n <- right, n `notElem` left]
    leftRec = ["direct left-recusion: " ++ n | n <- left, hasLeftRec n]
    unused = ["unused non-terminal: " ++ n
              | n <- left
              , n /= "__start"
              , n `notElem` right]
    hasLeftRec n = hasLeftRec' n /= []
    hasLeftRec' n = [0 | (Rule _ (x:_)) <- rule n rs, hasLeftRec'' n x]
    hasLeftRec'' n (NonTerm s) = n == s
    hasLeftRec'' _ _           = False
    left = Map.keys rs
    right = nub $ concat [nonTerm xs | n <- left, (Rule _ xs) <- rule n rs]
    nonTerm []               = []
    nonTerm (NonTerm s : xs) = s : nonTerm xs
    nonTerm (_:xs)           = nonTerm xs

-- |First set type
type FirstSet = Map.Map String [Rule]

-- |Compute the complete first set
firstSet :: RuleSet -> FirstSet
firstSet rs = Map.mapWithKey (\k _ -> find k rs) rs
  where
    find name rs = nub . sort $ concatMap compute $ noLeftRec $ rule name rs
    noLeftRec = filter (\(Rule a (x:_)) -> case x of
      NonTerm b -> a /= b
      _         -> True)
    compute (Rule _ [Empty]) = [Empty]
    compute (Rule name (x:xs)) = case compute x of
      [Empty] -> compute $ Rule name xs
      a       -> a
    compute a@(Term _) = [a]
    compute a@(TermToken _) = [a]
    compute (NonTerm s) = find s rs
    compute Empty = [Empty]

-- |Compute first set of a given rule
first :: Rule -> FirstSet -> [Rule]
first Empty _           = [Empty]
first a@(Term _) _      = [a]
first a@(TermToken _) _ = [a]
first (NonTerm s) fs    = fromMaybe [Empty] (Map.lookup s fs)
first (Rule _ (x:_)) fs = first x fs

-- |Separate rules into (parsing rules, lexing rules)
separate :: [Rule] -> ([Rule], [Rule])
separate rs = nonTermToToken (filter (not . hasRegex) rs, filter hasRegex rs)
  where
    hasRegex (Rule _ [])     = False
    hasRegex (Rule r (x:xs)) = hasRegex x || hasRegex (Rule r xs)
    hasRegex (NonTerm _)     = False
    hasRegex (Term _)        = False
    hasRegex (TermToken _)   = False
    hasRegex Empty           = False
    hasRegex (Concat [])     = False
    hasRegex (Concat (x:xs)) = hasRegex x || hasRegex (Concat xs)
    hasRegex (RegEx _)       = True
    hasRegex (RegExString _) = True

-- |Transform NonTerm into TermToken, when needed
nonTermToToken :: ([Rule], [Rule]) -> ([Rule], [Rule])
nonTermToToken (rs, lrs) = (mappers rs, mappers lrs)
    where
      mappers = map (\(Rule r xs) -> Rule r $ map (replaceNonTerm tok) xs)
      tok = map (\(Rule r _) -> r) lrs
      replaceNonTerm [] r = r
      replaceNonTerm (t:ts) r@(NonTerm nt) =
        if nt == t then TermToken t else replaceNonTerm ts r
      replaceNonTerm (_:ts) r = replaceNonTerm ts r

-- |Transform lexing rules to have only one RegEx on right
regexfy :: [Rule] -> [Rule]
regexfy lrs = concatMap replace lrs
  where
    replace (Rule r xs)    = [Rule r $ bind [RegEx ""] $ concatMap replace xs]
    replace (TermToken nt) = concatMap replace $ find nt
    replace x              = [x]
    bind acc [Empty]                      = acc ++ [Empty]
    bind (RegEx a:acc) (RegEx b:xs)       = bind [RegEx $ a ++ b] xs
    bind (RegEx a:acc) (RegExString b:xs) = bind [RegEx $ a ++ toRegex b] xs
    find r = let (Rule _ xs:_) = rule r rs in init xs
    toRegex s = '(' : concat [['[',c,']'] | c <- s] ++ ")"
    rs = ruleSet lrs
