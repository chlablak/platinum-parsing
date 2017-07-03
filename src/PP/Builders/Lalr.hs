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
import qualified Data.Map.Strict as Map
import           Data.Maybe
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
  collection rs fs = fusion (collection rs fs :: LrCollection Lr1Item)
  table c = case actions of
    Right act -> Right $ Map.union (Map.fromList act) (Map.fromList gotos)
    Left err  -> Left err
    where
      actions = let act = shifts ++ reduces ++ accepts in
        case conflict [] [] act of
          [] -> Right act
          xs -> Left xs
      conflict _ con [] = con
      conflict acc con ((k, v):xs) = case L.lookup k acc of
        Nothing -> conflict ((k,v):acc) con xs
        Just v2 -> conflict acc ((show k ++ " conflict: " ++ show v ++ " with " ++ show v2) : con) xs
      shifts = [((i, s), LrShift $ fromJust j)
              | i <- [0..(Vector.length c - 1)]
              , let is = c Vector.! i
              , s <- symbol is
              , term s
              , let j = next gs i s
              , isJust j]
      reduces = [((i, la), LrReduce r)
               | i <- [0..(Vector.length c - 1)]
               , let is = c Vector.! i
               , x@(LalrItem r@(Rule s _) _ la) <- reductibles is
               , s /= "__start"]
      accepts = [((i, Empty), LrAccept)
               | i <- [0..(Vector.length c - 1)]
               , let is = c Vector.! i
               , acc is]
      gotos = [((i, s), LrGoto $ fromJust j)
             | i <- [0..(Vector.length c - 1)]
             , let is = c Vector.! i
             , s <- symbol is
             , nonTerm s
             , let j = next gs i s
             , isJust j]
      term (Term _)      = True
      term (TermToken _) = True
      term _             = False
      reductibles is = [x | x <- Set.toList is, reductible x]
      reductible (LalrItem (Rule _ xs) p _) = L.length xs == p + 1
      acc = not . Set.null . Set.filter
        (\(LalrItem (Rule s _) p la) -> s == "__start" && p == 1 && la == Empty)
      nonTerm (NonTerm _) = True
      nonTerm _           = False
      gs = gotoSet c

-- |Construct the GOTO table
type GotoSet = Map.Map (Int, Rule) Int
gotoSet :: LrCollection LalrItem -> GotoSet
gotoSet c = Map.fromList [((i, s), fromJust j)
                        | i <- [0..(Vector.length c - 1)]
                        , let is = c Vector.! i
                        , s <- symbol is
                        , let j = goto c i s
                        , isJust j]

-- |Get the next items set
next :: GotoSet -> Int -> Rule -> Maybe Int
next gs i r = Map.lookup (i, r) gs

-- |Find the next possible symbols
symbol :: LrSet LalrItem -> [Rule]
symbol is = L.sort $ L.nub [x
                          | LalrItem (Rule _ xs) p _ <- Set.toList is
                          , let x = xs !! p
                          , x /= Empty]

-- |Find the next set
goto :: LrCollection LalrItem -> Int -> Rule -> Maybe Int
goto c i = goto' (c Vector.! i)
  where
    goto' is r = case list is r of
      []    -> Nothing
      (x:_) -> find $ inc x
    find x = Vector.findIndex (Set.member x) c
    list is r = [x | x <- Set.toList is, accept x r]
    accept (LalrItem (Rule _ xs) p _) r = xs !! p == r
    inc (LalrItem r p la) = LalrItem r (p + 1) la

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
