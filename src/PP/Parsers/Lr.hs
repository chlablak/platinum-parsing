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

import           PP.Builder (LrAction (..), LrTable (..), action, action')
import           PP.Parser  (LrParser (..))
import           PP.Rule    (Rule (..))

-- |Configuration for LR parser
data LrConfig = LrConfig
  { lrCount  :: Int        -- ^Counter
  , lrStack  :: [Int]      -- ^State stack
  , lrAction :: LrAction   -- ^Action to do
  , lrInput  :: String     -- ^Input
  } deriving (Eq, Show)

instance LrParser LrConfig where
  config t i = LrConfig 0 [0] (action' t 0 i) i
  next t (LrConfig c ss (LrShift s) (_:i)) =
    LrConfig (c + 1) (s : ss) (action' t s i) i
  next t (LrConfig c ss (LrReduce (Rule r xs)) i) =
    LrConfig (c + 1) sr (action t m $ NonTerm r) i
    where
      sr@(m:_) = drop (length xs - 1) ss
  next t (LrConfig c ss (LrGoto s) i) =
    LrConfig (c + 1) (s : ss) (action' t s i) i
  next _ c@(LrConfig _ _ LrError _)    = c
  next _ c@(LrConfig _ _ LrAccept _)   = c
  hasNext _ (LrConfig _ _ LrError _)  = False
  hasNext _ (LrConfig _ _ LrAccept _) = False
  hasNext _ _                         = True
