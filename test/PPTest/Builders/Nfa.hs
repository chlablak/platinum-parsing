module PPTest.Builders.Nfa (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import           PP.Builders.Nfa
import           PP.Grammars.Lexical
import           Test.Hspec

getNfa expr = let Right ast = (parseAst expr :: To RegExpr) in buildNfa ast

specs = describe "PPTest.Builders.Nfa" $ do

  it "should build the correct automaton ((a|b)*abb)" $ do
    -- Dragon Book, page 142, figure 3.34
    let expr = "(a|b)*abb"
    let e = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaNode),
                        (4,NfaNode),(5,NfaNode),(6,NfaNode),(7,NfaNode),
                        (8,NfaNode),(9,NfaNode),(10,NfaFinal)]
                       [(0,1,NfaEmpty),(0,7,NfaEmpty),(1,2,NfaEmpty),
                        (1,4,NfaEmpty),(2,3,NfaValue 'a'),(3,6,NfaEmpty),
                        (4,5,NfaValue 'b'),(5,6,NfaEmpty),(6,1,NfaEmpty),
                        (6,7,NfaEmpty),(7,8,NfaValue 'a'),(8,9,NfaValue 'b'),
                        (9,10,NfaValue 'b')]
    getNfa expr `shouldBe` e

  it "should build the correct automaton (a+)" $ do
    let expr = "a+"
    let e = Gr.mkGraph []
                       []
    getNfa expr `shouldBe` e

  it "should build the correct automaton (a?)" $ do
    let expr = "a?"
    let e = Gr.mkGraph []
                       []
    getNfa expr `shouldBe` e

  it "should build the correct automaton ([a-c])" $ do
    let expr = "[a-c]"
    let e = Gr.mkGraph []
                       []
    getNfa expr `shouldBe` e

  it "should build the correct automaton ([a-c0-2.-])" $ do
    let expr = "[a-c0-2.-]"
    let e = Gr.mkGraph []
                       []
    getNfa expr `shouldBe` e

  it "should build the correct automaton (.)" $ do
    let expr = "."
    let e = Gr.mkGraph []
                       []
    getNfa expr `shouldBe` e
