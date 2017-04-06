module Main where

import qualified PPTest.Grammars.EBNF as EBNF
import           Test.HUnit

tests = test [EBNF.tests]

main :: IO ()
main = do
  runTestTT tests
  return ()
