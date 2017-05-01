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
    ( LrTable
    , LrAction(..)
    , LrCollection
    , LrSet
    , LrBuilder(..)
    ) where

import           Data.Array
import           Data.Set
import           PP.Rule

-- |All LR parsers have the same table format
type LrTable = Array (Int, Rule) LrAction

-- |LR actions for a LR parser
data LrAction
  = LrShift Int
  | LrReduce Int
  | LrGoto Int
  | LrError String
  | LrAccept

-- |LR items set collection
type LrCollection set = Array Int set

-- |LR items set
type LrSet item = Set item

-- |LR parser common functions
class Ord item => LrBuilder item where
  -- |Build the items set collection
  collection :: RuleSet -> LrCollection (LrSet item)
  -- |Build the parsing table
  table :: LrCollection (LrSet item) -> LrTable
