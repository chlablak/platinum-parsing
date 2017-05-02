{-|
Module      : PP.Builder
Description : Common behavior for defined builders
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Builder
    ( LrTable(..)
    , LrAction(..)
    , LrCollection(..)
    , LrSet(..)
    , LrBuilder(..)
    ) where

import           Data.Map
import           Data.Set
import           Data.Vector
import           PP.Rule

-- |All LR parsers have the same table format
type LrTable = Map (Int, Rule) LrAction

-- |LR actions for a LR parser
data LrAction
  = LrShift Int
  | LrReduce Int
  | LrGoto Int
  | LrError
  | LrAccept
    deriving(Show, Eq)

-- |LR items set collection
type LrCollection item = Vector (LrSet item)

-- |LR items set
type LrSet item = Set item

-- |LR parser common functions
class Ord item => LrBuilder item where
  -- |Build the items set collection
  collection :: RuleSet -> LrCollection item
  -- |Build the parsing table
  table :: LrCollection item -> LrTable
