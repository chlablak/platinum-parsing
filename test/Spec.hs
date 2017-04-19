module Main where

import qualified PPTest.Grammars.Ebnf
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "PPTest" $ do
    PPTest.Grammars.Ebnf.specs
