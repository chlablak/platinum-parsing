module PPTest.Builders.Dfa (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import           PP.Builders.Dfa
import           PP.Builders.Nfa
import           PP.Grammars.Lexical
import           Test.Hspec

specs = describe "PPTest.Builders.Dfa" $

  it "should build the correct automaton (from NFA: (a|b)*abb)" $ do
    -- Dragon Book, page 142, figures 3.34 and 3.36
    let nfa = Gr.mkGraph [(0,NfaInitial),(1,NfaNode),(2,NfaNode),(3,NfaNode),
                          (4,NfaNode),(5,NfaNode),(6,NfaNode),(7,NfaNode),
                          (8,NfaNode),(9,NfaNode),(10,NfaFinal)]
                         [(0,1,NfaEmpty),(0,7,NfaEmpty),(1,2,NfaEmpty),
                          (1,4,NfaEmpty),(2,3,NfaValue 'a'),(3,6,NfaEmpty),
                          (4,5,NfaValue 'b'),(5,6,NfaEmpty),(6,1,NfaEmpty),
                          (6,7,NfaEmpty),(7,8,NfaValue 'a'),(8,9,NfaValue 'b'),
                          (9,10,NfaValue 'b')] :: NfaGraph
    let e = Gr.mkGraph [(0,DfaInitial),(1,DfaNode),(2,DfaNode),(3,DfaNode),
                        (4,DfaFinal)]
                       [(0,1,DfaValue 'a'),(0,2,DfaValue 'b'),(1,1,DfaValue 'a'),
                        (1,3,DfaValue 'b'),(2,1,DfaValue 'a'),(2,2,DfaValue 'b'),
                        (3,1,DfaValue 'a'),(3,4,DfaValue 'b'),(4,1,DfaValue 'a'),
                        (4,2,DfaValue 'b')]
    buildDfa nfa `shouldBe` e
