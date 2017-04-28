module PPTest.Rule (specs) where

import           PP
import           Test.Hspec

specs = describe "PPTest.Rule" $ do

  it "should uniformize correctly" $ do
    let r = [Rule "c" [Concat [Empty, Empty], Empty],
             Rule "a" [Rule "b" [Empty, Term 'c', Empty], Term 'c', Empty]]
    let e = [Rule "a" [NonTerm "b", Term 'c', Empty],
             Rule "b" [Term 'c', Empty],
             Rule "c" [Empty]]
    uniformize r `shouldBe` e

  it "should find the start rule and extend it" $ do
    let r = [Rule "a" [NonTerm "b", Empty],
             Rule "a" [Term 'c', Empty],
             Rule "b" [Term 'd', Empty]]
    let e = [Rule "__start" [NonTerm "a", Empty],
             Rule "a" [NonTerm "b", Empty],
             Rule "a" [Term 'c', Empty],
             Rule "b" [Term 'd', Empty]]
    extend r `shouldBe` Right e

  it "should detect when there is no start rule" $ do
    let r = [Rule "a" [NonTerm "b", Empty],
             Rule "a" [Term 'c', Empty],
             Rule "b" [Term 'd', NonTerm "c", Empty],
             Rule "c" [NonTerm "a", Empty]]
    extend r `shouldBe` Left "cannot extend, no start rule found (candidates: [])"

  it "should detect when there are many start rules" $ do
    let r = [Rule "a" [NonTerm "b", Empty],
             Rule "a" [Term 'c', Empty],
             Rule "b" [Term 'd', Empty],
             Rule "c" [NonTerm "b", Empty]]
    extend r `shouldBe` Left "cannot extend, no start rule found (candidates: [\"a\",\"c\"])"
