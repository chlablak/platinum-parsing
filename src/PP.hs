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
    , module PP.Lexer
    , module PP.Parser
    , module PP.Rule
    , module PP.Template
    ) where

import           PP.Builder
import           PP.Grammar
import           PP.Lexer
import           PP.Parser
import           PP.Rule
import           PP.Template
