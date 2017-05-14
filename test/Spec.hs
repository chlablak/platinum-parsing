module Main where

import qualified PPTest.Builders.Lalr
import qualified PPTest.Builders.Lr1
import qualified PPTest.Grammars.Ebnf
import qualified PPTest.Parsers.Lr
import qualified PPTest.Rule
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "PPTest" $ do
    PPTest.Grammars.Ebnf.specs
    PPTest.Rule.specs
    PPTest.Builders.Lr1.specs
    PPTest.Builders.Lalr.specs
    PPTest.Parsers.Lr.specs
