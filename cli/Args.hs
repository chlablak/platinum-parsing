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
data CommonArgs = CommonArgs
  { setLevel :: Int   -- ^Verbosity level
  , silent   :: Bool  -- ^Verbosity off
  }
    deriving Show

-- |Allowed commands
data CommandArgs
  = EbnfCmd EbnfArgs -- ^EBNF command
  | LalrCmd LalrArgs -- ^LALR command
    deriving Show

-- |EBNF command arguments
data EbnfArgs = EbnfArgs
  { ebnfFile     :: String   -- ^Input file
  , showMinified :: Bool     -- ^Print the minified grammar to output
  , showRules    :: Bool     -- ^Print the obtained rules
  , showFirstSet :: Bool     -- ^Print the first set
  , doCheck      :: Bool     -- ^Search for errors in grammar
  }
    deriving Show

-- |LALR command arguments
data LalrArgs = LalrArgs
  { lalrFile       :: String  -- ^Input file
  , showCollection :: Bool    -- ^Print the items sets collection
  , showSetI       :: Int     -- ^Print a specific items set
  , showTable      :: Bool    -- ^Print the LALR table
  , testWith       :: String  -- ^Test the LALR table on a source file
  , template       :: String  -- ^Specify a template
  }
    deriving Show
