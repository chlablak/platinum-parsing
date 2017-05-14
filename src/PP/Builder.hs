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
    , action
    , action'
    , LrAction(..)
    , LrCollection(..)
    , LrSet(..)
    , LrBuilder(..)
    ) where

import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import qualified Data.Vector     as Vector
import           PP.Rule

-- |All LR parsers have the same table format
type LrTable = Map.Map (Int, Rule) LrAction

-- |Get a LrAction from a LrTable
action :: LrTable -> Int -> Rule -> LrAction
action t i r    = fromMaybe LrError (Map.lookup (i, r) t)

-- |Get a LrAction from a LrTable
action' :: LrTable -> Int -> String -> LrAction
action' t i []    = action t i Empty
action' t i (x:_) = action t i $ Term x

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

-- |LR items set collection
type LrCollection item = Vector.Vector (LrSet item)

-- |LR items set
type LrSet item = Set.Set item

-- |LR parser common functions
class Ord item => LrBuilder item where
  -- |Build the items set collection
  collection :: RuleSet -> FirstSet -> LrCollection item
  -- |Build the parsing table
  table :: LrCollection item -> Either [String] LrTable
