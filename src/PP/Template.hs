{-|
Module      : PP.Template
Description : Common behavior for defined templates
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Template
    ( Template(..)
    ) where

import           Text.StringTemplate

class Template context where
  -- |Put the context into StringTemplate attributes
  attributes :: context -> StringTemplate String -> StringTemplate String
  -- |Compile a template with a given context
  compile :: context -> String -> String
  compile c t = render $ attributes c $ newSTMP t
