module PPTest.Templates.Dfa (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import           PP
import qualified PP.Templates.Dfa           as Dfa
import           Test.Hspec

specs = describe "PPTest.Templates.Dfa" $

  it "should compile correctly a template" $ do
    let dfa = Gr.mkGraph [(0,DfaInitial),(1,DfaNode),(2,DfaFinal "ab")]
                         [(0,1,DfaValue 'a'),(1,2,DfaValue 'b')]
    let t = "STATE $length(dfa.states)$\n\
            \$dfa.states:{state|$state.id$ $if(state.isInitial)$INITIAL$elseif(state.isNode)$NODE$else$FINAL $state.final$$endif$\n\
            \}$TRANSITION $length(dfa.transitions)$\n\
            \$dfa.transitions:{trans|$trans.from$ $trans.to$ $trans.symbol$\n\
            \}$"
    let e = "STATE 3\n\
            \0 INITIAL\n\
            \1 NODE\n\
            \2 FINAL ab\n\
            \TRANSITION 2\n\
            \0 1 a\n\
            \1 2 b\n"
    compile (Dfa.context dfa) t `shouldBe` e
