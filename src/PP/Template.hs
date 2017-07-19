{-|
Module      : PP.Template
Description : Common behavior for defined templates
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE FlexibleInstances #-}
module PP.Template
    ( Template(..)
    , mergeContext
    ) where

import           Text.StringTemplate

-- |Type class for Templatable structure
class Template context where
  -- |Put the context into StringTemplate attributes
  attributes :: context -> StringTemplate String -> StringTemplate String
  -- |Compile a template with a given context
  compile :: context -> String -> String
  compile c t = render $ attributes c $ newSTMP t

-- |Merge two contexts together
mergeContext :: (Template c1, Template c2) => c1 -> c2
                -> (StringTemplate String -> StringTemplate String)
mergeContext a b = attributes a . attributes b

-- |Allow to use `compile` with `mergeContext`
-- For example: `compile (mergeContext c1 c2) t`
instance Template ((->) (StringTemplate String) (StringTemplate String)) where
  attributes = id
