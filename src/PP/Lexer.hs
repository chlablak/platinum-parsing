{-|
Module      : PP.Lexer
Description : Common behavior for defined lexers
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Lexer
    ( IToken(..)
    , iToken
    , OToken(..)
    , Lexer(..)
    ) where

-- |Input token
data IToken
  = IToken0       -- ^Null token
  | IToken1 Char  -- ^Token value
    deriving (Eq, Ord)

instance Show IToken where
  show IToken0     = []
  show (IToken1 c) = [c]

-- |String to IToken
iToken :: String -> [IToken]
iToken s = map IToken1 s ++ [IToken0]

-- |Output token
data OToken
  = OToken0                  -- ^Null token
  | OToken1 String           -- ^Token name
  | OToken2 String [IToken]  -- ^Token name and value
    deriving (Show, Eq, Ord)

-- |Lexer class
class Lexer config where
  -- |Simulate the automaton on the input, for one iteration
  simulate :: config -> config
  -- |Check if the input is consumed
  consumed :: config -> Bool
  -- |Get the output tokens
  output :: config -> [OToken]
  -- |Consume the complete input
  consume :: config -> config
  consume c = if consumed c then c else consume $ simulate c
