{-|
Module      : PP.Builders.Lalr
Description : Builder for LALR parsers
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Builders.Lalr
    ( LalrItem(..)
    ) where

import qualified Data.List       as L
import qualified Data.Set        as Set
import qualified Data.Vector     as Vector
import           PP.Builder
import           PP.Builders.Lr1
import           PP.Rule

-- |LALR item
data LalrItem = LalrItem Rule Int Rule
  deriving (Eq, Ord)

instance Show LalrItem where
  show (LalrItem (Rule a xs) p la) =
    "[" ++ a ++ " -> " ++ right xs p ++ "; " ++ show la ++ "]"
    where
      right :: [Rule] -> Int -> String
      right [] _     = ""
      right xs 0     = "*," ++ right xs (-1)
      right [x] _    = show x
      right (x:xs) p = show x ++ "," ++ right xs (p - 1)

-- |LrBuilder instance for LalrItem
instance LrBuilder LalrItem where
  collection rs = fusion (collection rs :: LrCollection Lr1Item)
  table = undefined

-- |Compute the LALR collection from a LR(1) collection
fusion :: LrCollection Lr1Item -> LrCollection LalrItem
fusion lr1 = Vector.foldl' fusion' Vector.empty lalr
  where
    fusion' acc is = case core acc is of
      []  -> acc
      [_] -> Vector.snoc acc is
      xs  -> Vector.snoc acc (Set.unions xs)
    core acc is = [isx | isx <- Vector.toList lalr, same isx is, unique isx acc]
    unique is acc = L.null [0 | isx <- Vector.toList acc, same is isx]
    same isa isb = component isa == component isb
    component = Set.toList . Set.map (\(LalrItem r p _) -> (r, p))
    lalr = toLalrItem lr1

-- |Transform Lr1Item into LalrItem
toLalrItem :: LrCollection Lr1Item -> LrCollection LalrItem
toLalrItem = Vector.map (Set.map (\(Lr1Item r p la) -> LalrItem r p la))
