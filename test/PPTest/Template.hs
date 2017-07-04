module PPTest.Template (specs) where

import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Map.Strict            as Map
import           PP
import qualified PP.Templates.Dfa           as Dfa
import qualified PP.Templates.Lr            as Lr
import           Test.Hspec

specs = describe "PPTest.Template" $

  it "should be able to compile two contexts together" $ do
    let t = Map.singleton (0, Term 'a') LrAccept
    let dfa = Gr.mkGraph [(0,DfaInitial),(1,DfaFinal "f")]
                         [(0,1,DfaValue 'b')] :: DfaGraph
    let c1 = Lr.context t
    let c2 = Dfa.context dfa
    let st = "$length(lr.states)$ $length(dfa.states)$"
    compile (mergeContext c1 c2) st `shouldBe` "1 2"
