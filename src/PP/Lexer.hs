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
    ( IToken
    , OToken(..)
    , charLexer
    , Lexer(..)
    ) where

-- |Input token
type IToken = Char

-- |Output token
data OToken
  = OToken1 String           -- ^Token value
  | OToken2 [IToken] String  -- ^Token value and name
    deriving (Show, Eq, Ord)

-- |String to OToken
charLexer :: String -> [OToken]
charLexer = map (\c -> OToken1 [c])

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
