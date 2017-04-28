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
    , Lr1Set(..)
    ) where

import qualified Data.Array as Array
import qualified Data.Set   as Set
import           PP.Builder
import           PP.Rule

-- |LR(1) item
data Lr1Item = Lr1Item Rule Int Rule

-- |LR(1) items set
newtype Lr1Set = Lr1Set (Array.Array Int (Set.Set Lr1Item))

-- |LrBuilder instance for Lr1Set
instance LrBuilder Lr1Set where
  buildSet = undefined
  buildTable = undefined
