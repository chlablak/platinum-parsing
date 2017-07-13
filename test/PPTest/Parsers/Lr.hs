module PPTest.Parsers.Lr (specs) where

import qualified Data.List        as L
import           PP
import           PP.Builders.Lalr
import           PP.Parsers.Lr
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

specs = describe "PPTest.Parsers.Lr" $ do

  it "should build the first configuration" $ do
    let cfg = config t (charLexer "x*x+x") :: LrConfig
    cfg `shouldBe` LrConfig 0 [0] (LrShift 3) (charLexer "x*x+x") (LrAstRoot [])

  it "should parse a simple grammar correctly" $ do
    let e = [ LrConfig 0 [0] (LrShift 3) (charLexer "x*x+x")
              (LrAstRoot [])
            , LrConfig 1 [3,0] (LrReduce r6) (charLexer "*x+x")
              (LrAstRoot [LrAstTerm [OToken1 "x"]])
            , LrConfig 2 [0] (LrGoto 4) (charLexer "*x+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 3 [4,0] (LrShift 8) (charLexer "*x+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 4 [8,4,0] (LrShift 3) (charLexer "x+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"]])
            , LrConfig 5 [3,8,4,0] (LrReduce r6) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstTerm [OToken1 "x"]])
            , LrConfig 6 [8,4,0] (LrGoto 4) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 7 [4,8,4,0] (LrReduce r4) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 8 [8,4,0] (LrGoto 11) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]])
            , LrConfig 9 [11,8,4,0] (LrReduce r3) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]])
            , LrConfig 10 [0] (LrGoto 1) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]])
            , LrConfig 11 [1,0] (LrShift 6) (charLexer "+x")
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]])
            , LrConfig 12 [6,1,0] (LrShift 3) (charLexer "x")
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"]])
            , LrConfig 13 [3,6,1,0] (LrReduce r6) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstTerm [OToken1 "x"]])
            , LrConfig 14 [6,1,0] (LrGoto 4) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 15 [4,6,1,0] (LrReduce r4) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]])
            , LrConfig 16 [6,1,0] (LrGoto 1) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]])
            , LrConfig 17 [1,6,1,0] (LrReduce r2) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]])
            , LrConfig 18 [6,1,0] (LrGoto 9) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]])
            , LrConfig 19 [9,6,1,0] (LrReduce r1) []
              (LrAstRoot [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]])
            , LrConfig 20 [0] (LrGoto 5) []
              (LrAstRoot [LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]]])
            , LrConfig 21 [5,0] LrAccept []
              (LrAstRoot [LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]],LrAstTerm [OToken1 "*"],LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]],LrAstTerm [OToken1 "+"],LrAstNonTerm "E" [LrAstNonTerm "T" [LrAstNonTerm "F" [LrAstTerm [OToken1 "x"]]]]]])]
    let cfg = L.reverse $ parse' t $ config t (charLexer "x*x+x") :: [LrConfig]
    L.length cfg `shouldBe` 22
    cfg `shouldBe` e

  it "should detect an error in input" $ do
    let cfg = parse t $ config t (charLexer "x+x*()+x") :: LrConfig
    let (LrConfig _ _ s i _) = cfg
    s `shouldBe` LrError
    i `shouldBe` charLexer ")+x"
