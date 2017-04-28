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
    , LrBuilder(..)
    ) where

import qualified Data.Array as Array
import           PP.Rule

-- |All LR parsers have the same table format
newtype LrTable = LrTable (Array.Array (Int, Rule) LrAction)

-- |LR actions for a LR parser
data LrAction
  = LrShift Int
  | LrReduce Int
  | LrGoto Int
  | LrError String
  | LrAccept

-- |LR parser common functions
class LrBuilder set where
  buildSet :: RuleSet -> set    -- ^Build the items set
  buildTable :: set -> LrTable  -- ^Build the parsing table
