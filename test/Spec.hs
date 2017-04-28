module Main where

import qualified PPTest.Grammars.Ebnf
import qualified PPTest.Rule
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "PPTest" $ do
    PPTest.Grammars.Ebnf.specs
    PPTest.Rule.specs
