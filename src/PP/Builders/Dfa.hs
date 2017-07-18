{-|
Module      : PP.Builders.Dfa
Description : Builder for DFA
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PP.Builders.Dfa
    (
    ) where

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.List                  as L
import qualified Data.Map.Strict            as Map
import           Data.Maybe
import           PP.Builder

instance DfaBuilder NfaGraph where
  buildDfa nfa = removeDeadState $ Gr.mkGraph nodes edges
    where
      nodes = map (\k -> (k, findType k)) indices
      indices = L.nub $ map (\((k, _), _) -> k) ilist
      edges = map (\((k, NfaValue a), v) -> (k, v, DfaValue a)) ilist
      ilist = map (\((k, a), v) -> ((index k, a), index v)) list
      index = (Map.!) $ Map.fromList $ zip unique [0..]
      rindex = (Map.!) $ Map.fromList $ zip [0..] unique
      unique = L.nub $ map (\((k, _), _) -> k) list
      list = Map.toList $ buildSubSet nfa
      findType i = foldl findType' DfaNode $ map (toDfa . findType'') $ rindex i
      findType' DfaInitial _   = DfaInitial
      findType' _ DfaInitial   = DfaInitial
      findType' (DfaFinal n) _ = DfaFinal n
      findType' _ (DfaFinal n) = DfaFinal n
      findType' _ _            = DfaNode
      findType'' i = fromMaybe NfaNode $ Gr.lab nfa i
      toDfa NfaNode      = DfaNode
      toDfa NfaInitial   = DfaInitial
      toDfa (NfaFinal n) = DfaFinal n

-- |Build a transition table with "subset" algorithm
-- Dragon Book (2nd edition, fr), page 140, algorithm 3.20
buildSubSet :: NfaGraph -> Map.Map ([Gr.Node], NfaSymbol) [Gr.Node]
buildSubSet g = buildSubSet' (mark (emptyClosure [initial] g) Map.empty)
                             [emptyClosure [initial] g]         -- not marked
  where
    buildSubSet' acc []     = Map.filterWithKey isNotEmpty acc
    buildSubSet' acc (x:xs) = buildSubSet'' acc x xs (symbols g)
    buildSubSet'' acc _ xs [] = buildSubSet' acc xs
    buildSubSet'' acc x xs (a:as) =
      let u = emptyClosure (transition x a g) g in
        if Map.notMember (u, NfaEmpty) acc then
          buildSubSet'' (mark u $ ins x a u acc) x (u:xs) as
        else
          buildSubSet'' (ins x a u acc) x xs as
    ins d a = Map.insert (d, a)
    mark d = ins d NfaEmpty []
    initial = let [(i, _)] = filter isInitial (Gr.labNodes g) in i
    isInitial (_, NfaInitial) = True
    isInitial _               = False
    isNotEmpty (_, NfaEmpty) _ = False
    isNotEmpty _ _             = True

-- |Returns all symbols in the NFA
symbols :: NfaGraph -> [NfaSymbol]
symbols = L.sort . L.nub . map (\(_, _, v) -> v) . filter isValue . Gr.labEdges
  where
    isValue (_, _, NfaValue _) = True
    isValue _                  = False

-- |Find all nodes reachable by an empty symbol, for each starting nodes
emptyClosure :: [Gr.Node] -> NfaGraph -> [Gr.Node]
emptyClosure ns g = emptyClosure' ns ns
  where
    emptyClosure' acc []     = L.sort acc
    emptyClosure' acc (x:xs) = emptyClosure'' acc xs (suc x)
    emptyClosure'' acc ns []     = emptyClosure' acc ns
    emptyClosure'' acc ns (u:us) =
      if u `notElem` acc then
        emptyClosure'' (u:acc) (u:ns) us
      else
        emptyClosure'' acc ns us
    suc n = transition [n] NfaEmpty g

-- |Find successors nodes of starting nodes, linked by the symbol
transition :: [Gr.Node] -> NfaSymbol -> NfaGraph -> [Gr.Node]
transition ns s g = L.sort $ L.nub $ map fst $ filter (\(_, l) -> l == s) suc
  where
    suc = concat [Gr.lsuc g i | i <- ns]

-- |Remove dead states in the DFA
removeDeadState :: DfaGraph -> DfaGraph
removeDeadState g = Gr.labnfilter isNotDead g
  where
    isNotDead (n, DfaNode) =
      let (_, _, _, suc) = Gr.context g n in
        any (\(_, i) -> i /= n) suc
    isNotDead _            = True
