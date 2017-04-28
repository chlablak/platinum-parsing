{-|
Module      : PP
Description : Global import for all PP functionnalities
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable

This module imports all functionnalities of Platinum Parsing.
For more informations about this tool, please look at:
  https://github.com/chlablak/platinum-parsing
-}
module PP
    ( module PP.Builder
    , module PP.Grammar
    , module PP.Rule
    ) where

import           PP.Builder
import           PP.Grammar
import           PP.Rule
