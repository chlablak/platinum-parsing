module PPTest.Builders.Nfa (specs) where

import qualified Data.Char                  as C
import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import           PP.Builders.Nfa
import           PP.Grammars.Lexical
import           Test.Hspec

-- Utilities
getNfa expr = let Right ast = (parseAst expr :: To RegExpr) in buildNfa ast
isValue (_, _, NfaValue _) = True
isValue _                  = False

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
    let e = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaFinal)]
                       [(0,1,NfaEmpty),(1,2,NfaValue 'a'),
                        (2,1,NfaEmpty),(2,3,NfaEmpty)]
    getNfa expr `shouldBe` e

  it "should build the correct automaton (a?)" $ do
    let expr = "a?"
    let e = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaFinal)]
                       [(0,1,NfaEmpty),(0,3,NfaEmpty),
                        (1,2,NfaValue 'a'),(2,3,NfaEmpty)]
    getNfa expr `shouldBe` e

  it "should build the correct automaton ([a-c])" $ do
    let expr = "[a-c]"
    let e = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaNode),
                        (4,NfaNode),(5,NfaNode),(6,NfaNode),(7,NfaFinal)]
                       [(0,1,NfaEmpty),(0,3,NfaEmpty),(0,5,NfaEmpty),
                        (1,2,NfaValue 'a'),(2,7,NfaEmpty),(3,4,NfaValue 'b'),
                        (4,7,NfaEmpty),(5,6,NfaValue 'c'),(6,7,NfaEmpty)]
    getNfa expr `shouldBe` e

  it "should build the correct automaton ([a-c0-2.-])" $ do
    let expr = "[a-c0-2.-]"
    let e = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaNode),
                        (4,NfaNode),(5,NfaNode),(6,NfaNode),(7,NfaNode),
                        (8,NfaNode),(9,NfaNode),(10,NfaNode),(11,NfaNode),
                        (12,NfaNode),(13,NfaNode),(14,NfaNode),(15,NfaNode),
                        (16,NfaNode),(17,NfaFinal)]
                       [(0,1,NfaEmpty),(0,3,NfaEmpty),(0,5,NfaEmpty),
                        (0,7,NfaEmpty),(0,9,NfaEmpty),(0,11,NfaEmpty),
                        (0,13,NfaEmpty),(0,15,NfaEmpty),(1,2,NfaValue 'a'),
                        (2,17,NfaEmpty),(3,4,NfaValue 'b'),(4,17,NfaEmpty),
                        (5,6,NfaValue 'c'),(6,17,NfaEmpty),(7,8,NfaValue '0'),
                        (8,17,NfaEmpty),(9,10,NfaValue '1'),(10,17,NfaEmpty),
                        (11,12,NfaValue '2'),(12,17,NfaEmpty),(13,14,NfaValue '.'),
                        (14,17,NfaEmpty),(15,16,NfaValue '-'),(16,17,NfaEmpty)]
    getNfa expr `shouldBe` e

  it "should build the correct automaton (.)" $ do
    let expr = "."
    let e = [c | c <- [minBound..maxBound], C.isAscii c]
    let values = map (\(_, _, NfaValue c) -> c) $ filter isValue $ Gr.labEdges $ getNfa expr
    values `shouldBe` e
