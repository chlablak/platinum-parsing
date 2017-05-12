{-|
Module      : PP.Parser
Description : Common behavior for defined parsers
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Parser
    ( LrParser(..)
    ) where

import           PP.Builder (LrTable)

-- |Type class for LR parser
class LrParser config where
  -- |Put the input into the configuration
  config :: String -> config
  -- |Parse the configuration with a given LR table
  parse :: LrTable -> config -> Either String config
