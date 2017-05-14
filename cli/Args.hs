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
    , LalrArgs(..)
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
data CommandArgs
  = EbnfCmd EbnfArgs -- ^EBNF command
  | LalrCmd LalrArgs -- ^LALR command
  deriving Show

-- |EBNF command arguments
data EbnfArgs = EbnfArgs
  { file   :: String   -- ^Input file
  , minify :: Bool     -- ^Print the minified grammar to output
  , rules  :: Bool     -- ^Print the obtained rules
  , first  :: Bool     -- ^Print the first set
  , check  :: Bool     -- ^Search for errors in grammar
  }
    deriving Show

-- |LALR command arguments
data LalrArgs = LalrArgs
  { grammar    :: String  -- ^Input grammar (file)
  , collection :: Bool    -- ^Print the items sets collection
  , set        :: Int     -- ^Print a specific items set
  , table      :: Bool    -- ^Print the LALR table
  , testWith   :: String  -- ^Test the LALR table on a source file
  }
    deriving Show
