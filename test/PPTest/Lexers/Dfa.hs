module PPTest.Lexers.Dfa (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import           PP.Lexers.Dfa
import           Test.Hspec

specs = describe "PPTest.Lexers.Dfa" $ do

  it "should create a DFA from a list of lexical rules" $ do
    let rs = [Rule "digit" [RegEx "[0-2]", Empty],
              Rule "number" [TermToken "digit", RegEx "+", Empty]]
    let e = Gr.mkGraph [(0,DfaInitial),(1,DfaFinal "digit"),
                        (2,DfaFinal "digit"),(3,DfaFinal "digit"),
                        (4,DfaFinal "number"),(5,DfaFinal "number"),
                        (6,DfaFinal "number")]
                       [(0,1,DfaValue '0'),(0,2,DfaValue '1'),(0,3,DfaValue '2'),
                        (1,4,DfaValue '0'),(1,5,DfaValue '1'),(1,6,DfaValue '2'),
                        (2,4,DfaValue '0'),(2,5,DfaValue '1'),(2,6,DfaValue '2'),
                        (3,4,DfaValue '0'),(3,5,DfaValue '1'),(3,6,DfaValue '2'),
                        (4,4,DfaValue '0'),(4,5,DfaValue '1'),(4,6,DfaValue '2'),
                        (5,4,DfaValue '0'),(5,5,DfaValue '1'),(5,6,DfaValue '2'),
                        (6,4,DfaValue '0'),(6,5,DfaValue '1'),(6,6,DfaValue '2')]
    createDfa rs `shouldBe` e

  it "should consume a simple token correctly" $ do
    let rs = [Rule "p1" [RegEx "abb", Empty]]
    let dfa = createDfa rs
    let e = [OToken2 "abb" "p1"]
    let input = "abb"
    let config = dfaConfig input dfa
    output (consume config) `shouldBe` e

  it "should consume two tokens correctly" $ do
    -- Dragon Book (2nd edition, fr), page 156, example 3.29
    let rs = [Rule "p1" [RegEx "a", Empty],
              Rule "p2" [RegEx "abb", Empty],
              Rule "p3" [RegEx "a*b+", Empty]]
    let dfa = createDfa rs
    let e = [OToken2 "abb" "p2",
             OToken2 "a" "p1"]
    let input = "abba"
    let config = dfaConfig input dfa
    output (consume config) `shouldBe` e

  it "should ignore bad input" $ do
    let rs = [Rule "p1" [RegEx "a", Empty]]
    let dfa = createDfa rs
    let e = []
    let input = "bbb"
    let config = dfaConfig input dfa
    output (consume config) `shouldBe` e

  it "should skip whitespace" $ do
    let rs = [Rule "_type" [RegEx "int", Empty],
              Rule "id" [RegEx "[a-z]+", Empty]]
    let dfa = createDfa rs
    let e = [OToken2 "int" "_type",
             OToken2 "abc" "id"]
    let input = "\n  int  \t \n  abc   \n"
    let config = dfaConfig input dfa
    output (consume config) `shouldBe` e

  it "should keep all tokens, even when conflict" $ do
    let rs = [Rule "Id" [RegEx "([a-zA-Z]|_)([a-zA-Z]|_|[0-9])*", Empty],
              Rule "__token_int" [RegEx "([i][n][t])", Empty]]
    let dfa = createDfa rs
    let input = "i in int intt"
    let e = [OToken2 "i" "Id",
             OToken2 "in" "Id",
             OToken2 "int" "__token_int",
             OToken2 "intt" "Id"]
    let config = dfaConfig input dfa
    output (consume config) `shouldBe` e
