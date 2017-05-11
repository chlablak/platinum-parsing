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

import qualified Data.List   as L
import qualified Data.Set    as Set
import qualified Data.Vector as Vector
import           PP.Builder
import           PP.Rule

-- |LR(1) item
data Lr1Item = Lr1Item Rule Int Rule
  deriving (Eq, Ord)

instance Show Lr1Item where
  show (Lr1Item (Rule a xs) p la) =
    "[" ++ a ++ " -> " ++ right xs p ++ "; " ++ show la ++ "]"
    where
      right :: [Rule] -> Int -> String
      right [] _     = ""
      right xs 0     = "*," ++ right xs (-1)
      right [x] _    = show x
      right (x:xs) p = show x ++ "," ++ right xs (p - 1)

-- |LrBuilder instance for Lr1Item
instance LrBuilder Lr1Item where
  collection rs fs = collection' initialise
    where
      collection' c = case list c of
        [] -> c
        xs -> collection' $ c Vector.++ Vector.fromList xs
      list c = [g
              | is <- Vector.toList c
              , x <- symbol is
              , let g = goto is x rs fs
              , accept g c]
      accept is c = not (Set.null is) && Vector.notElem is c
      symbol is = L.nub [x
                       | Lr1Item (Rule _ xs) p _ <- Set.toList is
                       , let x = xs !! p
                       , x /= Empty]
      initialise =
        Vector.singleton $ closure (Set.singleton start) rs fs
      start = Lr1Item (head $ rule "__start" rs) 0 Empty

  -- |Not impl. yet
  table = undefined

-- |Compute the closure of a items set
closure :: LrSet Lr1Item -> RuleSet -> FirstSet -> LrSet Lr1Item
closure is rs fs = case list is rs fs of
  [] -> is
  xs -> Set.union is $ closure (Set.fromList xs) rs fs
  where
    list is rs fs = [Lr1Item r 0 t
                   | i <- Set.toList is
                   , r <- rule (next i) rs
                   , t <- term i fs]
    next (Lr1Item (Rule _ xs) pos _) = case xs !! pos of
      (NonTerm r) -> r
      _           -> ""
    term (Lr1Item (Rule _ xs) pos la) fs = case xs !! (pos + 1) of
      Empty -> first la fs
      r     -> first r fs

-- |Compute the GOTO of a items set for a given rule
goto :: LrSet Lr1Item -> Rule -> RuleSet -> FirstSet -> LrSet Lr1Item
goto is r = closure $ Set.fromList [inc i | i <- Set.toList is, accept i r]
  where
    inc (Lr1Item r p la) = Lr1Item r (p + 1) la
    accept (Lr1Item (Rule _ xs) p _) r = xs !! p == r
