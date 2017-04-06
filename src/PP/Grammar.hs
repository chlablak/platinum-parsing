{-|
Module      : PP.Grammar
Description : Grammar parsing
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Grammar
    ( parse
    ) where

import           PP.Grammars
import qualified PP.Grammars.EBNF as EBNF

instance InputGrammar EBNF.Syntax where
  parser = EBNF.syntax
