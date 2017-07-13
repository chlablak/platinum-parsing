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
    , NewArgs(..)
    , BuildArgs(..)
    ) where

-- |Global arguments
data Args = Args CommonArgs CommandArgs
  deriving Show

-- |Common arguments
data CommonArgs = CommonArgs
  { setLevel :: Int     -- ^Verbosity level
  , silent   :: Bool    -- ^Verbosity off
  , useWork  :: Bool    -- ^Use '.pp-work/' directory
  , path     :: String  -- ^Working directory path
  }
    deriving Show

-- |Allowed commands
data CommandArgs
  = EbnfCmd EbnfArgs    -- ^EBNF command
  | LalrCmd LalrArgs    -- ^LALR command
  | NewCmd NewArgs      -- ^New command
  | BuildCmd BuildArgs  -- ^Build command
    deriving Show

-- |EBNF command arguments
data EbnfArgs = EbnfArgs
  { ebnfFile      :: String   -- ^Input file
  , showMinified  :: Bool     -- ^Print the minified grammar to output
  , showRules     :: Bool     -- ^Print the obtained rules
  , showFirstSet  :: Bool     -- ^Print the first set
  , doCheck       :: Bool     -- ^Search for errors in grammar
  , showLexical   :: Bool     -- ^Print lexical rules
  , showRegexfied :: Bool     -- ^Print the regexfied lexical rules
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
  , showDfa        :: Bool    -- ^Print the DFA
  , showAst        :: Bool    -- ^Print the parsed AST
  , astHtml        :: String  -- ^Output the AST to HTML list
  }
    deriving Show

-- |New command arguments
newtype NewArgs = NewArgs
  { projectName :: String   -- ^Project name
  }
    deriving (Show)

-- |Build command arguments
data BuildArgs = BuildArgs
  { disableTemplate :: Bool   -- ^Disable template compilation
  , disableTest     :: Bool   -- ^Disable tests
  , buildTestWith   :: String -- ^Test the LALR table on a source file
  , buildShowAst    :: Bool   -- ^Print the parsed AST
  , buildAstHtml    :: String -- ^Output the AST to HTML list
  }
    deriving (Show)
