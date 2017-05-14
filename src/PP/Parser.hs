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
  config :: LrTable -> String -> config
  -- |Parse one iteration only
  next :: LrTable -> config -> config
  -- |Check if there is still an iteration
  hasNext :: LrTable -> config -> Bool
  -- |Parse all iterations
  parse :: LrTable -> config -> config
  parse t c | hasNext t c = parse t $ next t c
            | otherwise = c
  -- |Parse all iterations and keep all configurations (in reverse order)
  parse' :: LrTable -> config -> [config]
  parse' t c = parse'' t [c]
    where
      parse'' t acc@(c:_) | hasNext t c = parse'' t $ next t c : acc
                          | otherwise = acc
