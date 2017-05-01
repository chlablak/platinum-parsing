{-|
Module      : PP.Builders.Lr1
Description : Builder for LR(1) parsers
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Builders.Lr1
    ( Lr1Item(..)
    ) where

import qualified Data.Set   as Set
import           PP.Builder
import           PP.Rule

-- |LR(1) item
data Lr1Item = Lr1Item Rule Int Rule
  deriving (Eq, Ord, Show, Read)

-- |LrBuilder instance for Lr1Set
instance LrBuilder Lr1Item where
  collection = undefined
  table = undefined

-- |Compute the closure of a items set
closure :: LrSet Lr1Item -> RuleSet -> FirstSet -> LrSet Lr1Item
closure is rs fs = case list is rs fs of
  [] -> is
  xs -> Set.union is (closure (Set.fromList xs) rs fs)
  where
    list is rs fs = [Lr1Item r 0 t |
      i <- Set.toList is,
      r <- rule (next i) rs,
      t <- term i fs]
    next (Lr1Item (Rule _ xs) pos _) = case xs !! pos of
      (NonTerm r) -> r
      _           -> ""
    term (Lr1Item (Rule _ xs) pos la) fs = case xs !! (pos + 1) of
      Empty -> first la fs
      r     -> first r fs

-- |Compute the GOTO of a items set for a given rule
goto :: LrSet Lr1Item -> Rule -> RuleSet -> FirstSet -> LrSet Lr1Item
goto is r = closure (Set.fromList [inc i | i <- Set.toList is, accept i r])
  where
    inc (Lr1Item r p la) = Lr1Item r (p + 1) la
    accept (Lr1Item (Rule _ xs) p _) r = xs !! p == r
