{-|
Module      : PP.Templates.Dfa
Description : DFA template
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE DeriveDataTypeable #-}
module PP.Templates.Dfa
    ( DfaContext
    , context
    ) where

import           Data.Data
import qualified Data.Graph.Inductive.Graph          as Gr
import           Data.Typeable
import           PP.Builder
import           PP.Template
import           Text.StringTemplate
import           Text.StringTemplate.GenericStandard

-- |DFA context
data DfaContext = DfaContext
  { states      :: [DfaContextState]      -- ^DFA states (nodes)
  , transitions :: [DfaContextTransition] -- ^DFA transitions (links)
  } deriving (Data, Typeable, Eq)
data DfaContextState = DfaContextState
  { id        :: Int                      -- ^State ID
  , isInitial :: Bool                     -- ^Is it initial node ?
  , isNode    :: Bool                     -- ^Is it middle node ?
  , isFinal   :: Bool                     -- ^Is it final node ?
  , final     :: String                   -- ^Final node value
  } deriving (Data, Typeable, Eq)
data DfaContextTransition = DfaContextTransition
  { from   :: Int                         -- ^State from
  , to     :: Int                         -- ^State to
  , symbol :: Char                        -- ^Transition symbol
  } deriving (Data, Typeable, Eq)

-- |Construct DFA context
context :: DfaGraph -> DfaContext
context dfa = DfaContext states' transitions'
  where
    states' = map fromNode $ Gr.labNodes dfa
    transitions' = map fromEdge $ Gr.labEdges dfa
    fromNode (i, DfaInitial) = DfaContextState i True False False ""
    fromNode (i, DfaNode)    = DfaContextState i False True False ""
    fromNode (i, DfaFinal f) = DfaContextState i False False True f
    fromEdge (i, j, DfaValue s) = DfaContextTransition i j s

-- |Template instance for DfaContext
instance Template DfaContext where
  attributes = setAttribute "dfa"
