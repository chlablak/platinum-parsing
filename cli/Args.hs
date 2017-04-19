{-|
Module      : Args
Description : CLI arguments record
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Args
    ( Args(..)
    , CommonArgs(..)
    , CommandArgs(..)
    , EbnfArgs(..)
    ) where

-- |Global arguments
data Args = Args CommonArgs CommandArgs
  deriving Show

-- |Common arguments
newtype CommonArgs = CommonArgs
  { verbose :: Bool -- ^Verbose output
  }
    deriving Show

-- |Allowed commands
newtype CommandArgs
  = EbnfCmd EbnfArgs -- ^EBNF command
  deriving Show

-- |EBNF command arguments
data EbnfArgs = EbnfArgs
  { file   :: String   -- ^Input file
  , minify :: Bool     -- ^Print the minified grammar to output
  }
    deriving Show
