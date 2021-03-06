module PPTest.Rule (specs) where

import           Data.Map.Strict (toList)
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

  it "should generate the correct RuleSet" $ do
    let r = [Rule "__start" [NonTerm "a", Empty],
             Rule "a" [Term 'b', Empty],
             Rule "a" [NonTerm "c", Empty],
             Rule "c" [Term 'd', Empty],
             Rule "c" [Empty],
             Rule "e" [Empty]]
    let e = [("__start", [[NonTerm "a", Empty]]),
             ("a", [[Term 'b', Empty], [NonTerm "c", Empty]]),
             ("c", [[Term 'd', Empty], [Empty]]),
             ("e", [[Empty]])]
    toList (ruleSet r) `shouldBe` e

  it "should generate the correct FirstSet" $ do
    let r = [Rule "__start" [NonTerm "A", Empty],
             Rule "A" [NonTerm "B", Empty],
             Rule "A" [Term 'a', Empty],
             Rule "B" [Term 'b', Empty],
             Rule "B" [NonTerm "C", NonTerm "D", Empty],
             Rule "C" [Term 'c', Empty],
             Rule "C" [Empty],
             Rule "D" [Term 'd', Empty]]
    let e = [("A", [Term 'a', Term 'b', Term 'c', Empty]),
             ("B", [Term 'b', Term 'c', Empty]),
             ("C", [Term 'c', Empty]),
             ("D", [Term 'd']),
             ("__start", [Term 'a', Term 'b', Term 'c', Empty])]
    toList (firstSet (ruleSet r)) `shouldBe` e

  it "should handle left recursion (firstSet)" $ do
      let r = [Rule "__start" [NonTerm "E", Empty],
               Rule "E" [NonTerm "E", Term '+', NonTerm "T", Empty],
               Rule "E" [NonTerm "T", Empty],
               Rule "T" [NonTerm "T", Term '*', NonTerm "F", Empty],
               Rule "T" [NonTerm "F", Empty],
               Rule "F" [Term '(', NonTerm "E", Term ')', Empty],
               Rule "F" [Term 'x', Empty]]
      let e = [("E", [Term '(', Term 'x']),
               ("F", [Term '(', Term 'x']),
               ("T", [Term '(', Term 'x']),
               ("__start", [Term '(', Term 'x'])]
      toList (firstSet (ruleSet r)) `shouldBe` e

  it "should check a rules set for missing non-terminals" $ do
    let r = [Rule "__start" [NonTerm "A", Empty],
             Rule "A" [NonTerm "B", NonTerm "C", Empty],
             Rule "C" [Empty]]
    let e = (["missing non-terminal: B"], [])
    check (ruleSet r) `shouldBe` e

  it "should check a rules set for unused non-terminals" $ do
    let r = [Rule "__start" [NonTerm "A", Empty],
             Rule "A" [NonTerm "B", Empty],
             Rule "B" [Empty],
             Rule "C" [Empty]]
    let e = ([], ["unused non-terminal: C"])
    check (ruleSet r) `shouldBe` e

  it "should check a rules set for direct left recursion" $ do
    let r = [Rule "__start" [NonTerm "A", Empty],
             Rule "A" [NonTerm "B", Empty],
             Rule "B" [NonTerm "B", Term 'b', Empty]]
    let e = (["direct left-recusion: B"], [])
    check (ruleSet r) `shouldBe` e

  it "should check a rules set for indirect left recursion" $
    pendingWith "not impl. yet"

  it "should separate parsing and lexing rules" $ do
    let r = [Rule "a" [NonTerm "b", Empty],
             Rule "c" [RegEx "d", Empty]]
    let e = ([Rule "a" [NonTerm "b", Empty]],
             [Rule "c" [RegEx "d", Empty]])
    separate r `shouldBe` e

  it "should transform lexical rules to have only one regex on right" $ do
    let r = [Rule "a" [TermToken "b", Empty],
             Rule "b" [TermToken "c", RegEx "bb", Empty],
             Rule "c" [RegEx "cc", TermToken "d", Empty],
             Rule "d" [RegEx "dd", RegEx "dd", Empty]]
    let e = [Rule "a" [RegEx "ccddddbb", Empty],
             Rule "b" [RegEx "ccddddbb", Empty],
             Rule "c" [RegEx "ccdddd", Empty],
             Rule "d" [RegEx "dddd", Empty]]
    regexfy r `shouldBe` e

  it "should remove unused lexical rules" $ do
    let r = [Rule "A" [NonTerm "Number", Empty],
             Rule "Number" [NonTerm "Digit", RegEx "+", Empty],
             Rule "Digit" [RegEx "[0-9]", Empty]]
    let e = [Rule "Number" [RegEx "[0-9]+", Empty]]
    let (rs, lrs) = separate r
    removeUnusedToken (ruleSet rs) (regexfy lrs) `shouldBe` e
