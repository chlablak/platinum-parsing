module Main where

import qualified PPTest.Grammars.EBNF
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "PPTest" $ do
    PPTest.Grammars.EBNF.specs
