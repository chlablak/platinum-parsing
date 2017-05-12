{-|
Module      : PP.Parsers.Lr
Description : LR parser
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Parsers.Lr
    ( LrConfig(..)
    ) where

import qualified Data.Map.Strict as Map
import           PP.Builder      (LrAction (..), LrTable (..), action)
import           PP.Parser       (LrParser (..))
import           PP.Rule         (Rule (..))

-- |Configuration for LR parser
data LrConfig = LrConfig
  { lrStack :: [Int]    -- ^State stack
  , lrInput :: String   -- ^Input
  } deriving (Eq, Show)

instance LrParser LrConfig where
  config = LrConfig [0]
  parse = parse'

-- |LrParser.parse implementation
parse' :: LrTable -> LrConfig -> Either String LrConfig
parse' t c = undefined
