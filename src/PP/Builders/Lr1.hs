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

import qualified Data.List  as L
import qualified Data.Set   as S
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
  xs -> S.union is (closure (S.fromList xs) rs fs)
  where
    list is rs fs = [Lr1Item r 0 t |
      i <- S.toList is,
      r <- rule (next i) rs,
      t <- term i fs]
    next (Lr1Item (Rule _ xs) pos _) = case xs !! pos of
      (NonTerm r) -> r
      _           -> ""
    term (Lr1Item (Rule _ xs) pos la) fs = case xs !! (pos + 1) of
      Empty -> first la fs
      r     -> first r fs
