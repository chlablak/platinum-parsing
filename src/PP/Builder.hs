{-|
Module      : PP.Builder
Description : Common behavior for defined builders
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module PP.Builder
    ( -- *(LA)LR
      LrTable(..)
    , action
    , action'
    , LrAction(..)
    , LrCollection(..)
    , LrSet(..)
    , LrBuilder(..)
      -- *NFA
    , NfaGraph(..)
    , NfaNode(..)
    , NfaSymbol(..)
    , NfaBuilder(..)
      -- *DFA
    , DfaGraph(..)
    , DfaNode(..)
    , DfaSymbol(..)
    , DfaBuilder(..)
    ) where

import           Control.Monad
import           Data.Binary
import qualified Data.Graph.Inductive.Graph        as Gr
import qualified Data.Graph.Inductive.PatriciaTree as Gr
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Set                          as Set
import qualified Data.Vector                       as Vector
import           PP.Lexer                          (OToken (..))
import           PP.Rule

-- |All LR parsers have the same table format
type LrTable = Map.Map (Int, Rule) LrAction

-- |Get a LrAction from a LrTable (Rule version)
action :: LrTable -> Int -> Rule -> LrAction
action t i r    = fromMaybe LrError (Map.lookup (i, r) t)

-- |Get a LrAction from a LrTable (OToken version)
action' :: LrTable -> Int -> [OToken] -> LrAction
action' t i []                = action t i Empty
action' t i (OToken1 []:_)    = action t i Empty
action' t i (OToken1 (x:_):_) = action t i $ Term x
action' t i (OToken2 _ s:_)   = action t i $ TermToken s

-- |LR actions for a LR parser
data LrAction
  = LrShift Int
  | LrReduce Rule
  | LrGoto Int
  | LrError
  | LrAccept
    deriving(Eq)

instance Show LrAction where
  show (LrShift i)  = "shift " ++ show i
  show (LrReduce r) = "reduce " ++ show r
  show (LrGoto i)   = "goto " ++ show i
  show LrError      = "error"
  show LrAccept     = "accept"

instance Binary LrAction where
  put (LrShift i)  = putWord8 0 >> put i
  put (LrReduce r) = putWord8 1 >> put r
  put (LrGoto i)   = putWord8 2 >> put i
  put LrError      = putWord8 3
  put LrAccept     = putWord8 4
  get = do
    tag <- getWord8
    case tag of
      0 -> fmap LrShift get
      1 -> fmap LrReduce get
      2 -> fmap LrGoto get
      3 -> return LrError
      4 -> return LrAccept

-- |LR items set collection
type LrCollection item = Vector.Vector (LrSet item)

instance (Binary item) => Binary (LrCollection item) where
  put c = put $ Vector.toList c
  get = fmap Vector.fromList get

-- |LR items set
type LrSet item = Set.Set item

-- |LR parser common functions
class Ord item => LrBuilder item where
  -- |Build the items set collection
  collection :: RuleSet -> FirstSet -> LrCollection item
  -- |Build the parsing table
  table :: LrCollection item -> Either [String] LrTable

-- |Nondeterministic finite automaton (NFA)
type NfaGraph = Gr.Gr NfaNode NfaSymbol
data NfaNode = NfaInitial | NfaNode | NfaFinal String deriving (Eq, Ord, Show, Read)
data NfaSymbol = NfaValue Char | NfaEmpty deriving (Eq, Ord, Show, Read)

-- |NFA builders
class NfaBuilder from where
  buildNfa :: from -> NfaGraph
  buildNfa' :: String -> from -> NfaGraph

-- |Deterministic finite automaton (DFA)
type DfaGraph = Gr.Gr DfaNode DfaSymbol
data DfaNode = DfaInitial | DfaNode | DfaFinal String deriving (Eq, Ord, Show, Read)
newtype DfaSymbol = DfaValue Char deriving (Eq, Ord, Read)

instance Show DfaSymbol where
  show (DfaValue c) = show c

instance Binary DfaGraph where
  put g = put (Gr.labNodes g) >> put (Gr.labEdges g)
  get = liftM2 Gr.mkGraph get get

instance Binary DfaNode where
  put DfaInitial   = putWord8 0
  put DfaNode      = putWord8 1
  put (DfaFinal s) = putWord8 2 >> put s
  get = do
    tag <- getWord8
    case tag of
      0 -> return DfaInitial
      1 -> return DfaNode
      2 -> fmap DfaFinal get

instance Binary DfaSymbol where
  put (DfaValue c) = put c
  get = fmap DfaValue get

-- |DFA builders
class DfaBuilder from where
  buildDfa :: from -> DfaGraph
