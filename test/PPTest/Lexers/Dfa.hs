module PPTest.Lexers.Dfa (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import           PP.Lexers.Dfa
import           Test.Hspec

specs = describe "PPTest.Lexers.Dfa" $ do

  it "should create a DFA from a list of lexical rules" $ do
    let rs = [Rule "digit" [RegEx "[0-2]", Empty],
              Rule "number" [NonTerm "digit", RegEx "+", Empty]]
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

  it "should consume a simple input correctly" $ do
    let rs = [Rule "abb" [RegEx "abb", Empty]]
    let dfa = createDfa rs
    let e = [OToken2 "abb" [IToken1 'a', IToken1 'b', IToken1 'b']]
    let input = "abb"
    let config = dfaConfig input dfa
    let c2 = consume config
    output (consume config) `shouldBe` e
