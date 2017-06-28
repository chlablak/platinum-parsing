{-|
Module      : PP.Builders.Nfa
Description : Builder for NFA
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Builders.Nfa
    (
    ) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP.Builder
import           PP.Grammars.Lexical

-- |Build a NFA from a RegExpr
instance NfaBuilder RegExpr where
  buildNfa (RegExpr [])   = buildSym NfaEmpty
  buildNfa (RegExpr [x])  = buildNfa x
  buildNfa (RegExpr xs)   = union $ map buildNfa xs
  buildNfa (Choice [])    = buildSym NfaEmpty
  buildNfa (Choice [x])   = buildNfa x
  buildNfa (Choice xs)    = foldl1 concatenate $ map buildNfa xs
  buildNfa (Many0 x)      = kleeneStar $ buildNfa x
  buildNfa (Many1 x)      = undefined
  buildNfa (Option x)     = undefined
  buildNfa (Group x)      = buildNfa x
  buildNfa (Class xs)     = undefined
  buildNfa (Interval a b) = undefined
  buildNfa (Value c)      = buildSym $ NfaValue c
  buildNfa Any            = undefined

-- |Build a simple NFA
buildSym :: NfaSymbol -> NfaGraph
buildSym s = Gr.mkGraph [(0,NfaInitial),(1,NfaFinal)] [(0,1,s)]

-- |Concatenate two NFA
concatenate :: NfaGraph -> NfaGraph -> NfaGraph
concatenate a b = Gr.mkGraph (an2 ++ bn) (ae ++ be)
  where
    an2 = map (\n@(i, _) -> if i == final then (i, NfaNode) else n) an
    bn = map (\(i, n) -> (i + final, n)) $ filter isNotInitial $ Gr.labNodes b
    ae = Gr.labEdges a
    be = map (\(i, j, e) -> (i + final, j + final, e)) $ Gr.labEdges b
    final = let [(i, _)] = filter isFinal an in i
    an = Gr.labNodes a
    isFinal (_, NfaFinal) = True
    isFinal _             = False
    isNotInitial (_, NfaInitial) = False
    isNotInitial _               = True

-- |Union a list of NFA
union :: [NfaGraph] -> NfaGraph
union gs = Gr.mkGraph (nodesU ++ nodes3) (edgesU ++ edges2)
  where
    nodes3 = map (\(i, _) -> (i, NfaNode)) nodes2
    nodesU = [(0,NfaInitial),(final,NfaFinal)]
    edgesU = [ (i,j,NfaEmpty)
             | n <- nodes2
             , isNotNode n
             , let (i,j) = getIJ n]
    nodes2 = concat $ add $ zip diff nodes
    edges2 = concat $ adde $ zip diff edges
    nodes = map Gr.labNodes gs
    edges = map Gr.labEdges gs
    getIJ (j, NfaInitial) = (0, j)
    getIJ (i, NfaFinal)   = (i, final)
    final = last diff
    diff = diff' nodes 1
    diff' [] d     = [d]
    diff' (x:xs) d = d : diff' xs (d + length x)
    add = map add'
    add' (d, xs) = map (add'' d) xs
    add'' d (i, n) = (i + d, n)
    adde = map adde'
    adde' (d, xs) = map (adde'' d) xs
    adde'' d (i, j, n) = (i + d, j + d, n)
    isNotNode (_, NfaNode) = False
    isNotNode _            = True

-- |For a NFA `x`, returns the NFA for `x*` (Kleene star)
kleeneStar :: NfaGraph -> NfaGraph
kleeneStar g = Gr.mkGraph (nodes2 ++ nodesK) (edges2 ++ edgesK)
  where
    nodesK = [(initial-1,NfaInitial),(final+1,NfaFinal)]
    edgesK = [(initial-1,initial,NfaEmpty),
              (final,final+1,NfaEmpty),
              (initial-1,final+1,NfaEmpty)]
    nodes2 = map (\(i, _) -> (i, NfaNode)) nodes
    edges2 = (final,initial,NfaEmpty) : edges
    final = let [(i, _)] = filter isFinal nodes in i
    initial = let [(i, _)] = filter isInitial nodes in i
    nodes = map (\(i, n) -> (i + 1, n)) $ Gr.labNodes g
    edges = map (\(i, j, e) -> (i + 1, j + 1, e)) $ Gr.labEdges g
    isFinal (_, NfaFinal) = True
    isFinal _             = False
    isInitial (_, NfaInitial) = True
    isInitial _               = False
