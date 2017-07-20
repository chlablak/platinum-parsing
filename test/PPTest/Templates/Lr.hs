module PPTest.Templates.Lr (specs) where

import qualified Data.List        as L
import           PP
import           PP.Builders.Lalr
import qualified PP.Templates.Lr  as Lr
import           System.IO
import           Test.Hspec

r0 = Rule "__start" [NonTerm "E", Empty]
r1 = Rule "E" [NonTerm "T", Term '+', NonTerm "E", Empty]
r2 = Rule "E" [NonTerm "T", Empty]
r3 = Rule "T" [NonTerm "F", Term '*', NonTerm "T", Empty]
r4 = Rule "T" [NonTerm "F", Empty]
r5 = Rule "F" [Term '(', NonTerm "E", Term ')', Empty]
r6 = Rule "F" [Term 'x', Empty]
rs = ruleSet [r0, r1, r2, r3, r4, r5, r6]
fs = firstSet rs
c = collection rs fs :: LrCollection LalrItem
Right t = table c

specs = describe "PPTest.Templates.Lr" $

  it "should compile correctly a template" $ do
    te <- readFile "test/res/lr-table"
    e <- readFile "test/res/lr-table.test"
    compile (Lr.context t) te `shouldBe` e
