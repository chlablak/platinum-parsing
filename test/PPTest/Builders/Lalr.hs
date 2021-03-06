module PPTest.Builders.Lalr (specs) where

import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Data.Vector      as Vector
import           PP
import           PP.Builders.Lalr
import           Test.Hspec

specs = describe "PPTest.Builders.Lalr" $ do

  it "should build the LALR items set collection" $ do
    -- Dragon Book (2nd edition, fr), page 240, example 4.54
    let rs = ruleSet [Rule "__start" [NonTerm "S", Empty],
                      Rule "S" [NonTerm "C", NonTerm "C", Empty],
                      Rule "C" [Term 'c', NonTerm "C", Empty],
                      Rule "C" [Term 'd', Empty]]
    let fs = firstSet rs
    let c = collection rs fs :: LrCollection LalrItem
    let e0 = [LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'c'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'd'),
              LalrItem (Rule "C" [Term 'd',Empty]) 0 (Term 'c'),
              LalrItem (Rule "C" [Term 'd',Empty]) 0 (Term 'd'),
              LalrItem (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 0 Empty,
              LalrItem (Rule "__start" [NonTerm "S",Empty]) 0 Empty]
    let e1 = [LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'c'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'd'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 Empty,
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 (Term 'c'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 (Term 'd'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 Empty,
              LalrItem (Rule "C" [Term 'd',Empty]) 0 (Term 'c'),
              LalrItem (Rule "C" [Term 'd',Empty]) 0 (Term 'd'),
              LalrItem (Rule "C" [Term 'd',Empty]) 0 Empty]
    let e2 = [LalrItem (Rule "C" [Term 'd',Empty]) 1 (Term 'c'),
              LalrItem (Rule "C" [Term 'd',Empty]) 1 (Term 'd'),
              LalrItem (Rule "C" [Term 'd',Empty]) 1 Empty]
    let e3 = [LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 Empty,
              LalrItem (Rule "C" [Term 'd',Empty]) 0 Empty,
              LalrItem (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 1 Empty]
    let e4 = [LalrItem (Rule "__start" [NonTerm "S",Empty]) 1 Empty]
    let e5 = [LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 (Term 'c'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 (Term 'd'),
              LalrItem (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 Empty]
    let e6 = [LalrItem (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 2 Empty]
    Vector.length c `shouldBe` 7
    Set.toList (c Vector.! 0) `shouldBe` e0
    Set.toList (c Vector.! 1) `shouldBe` e1
    Set.toList (c Vector.! 2) `shouldBe` e2
    Set.toList (c Vector.! 3) `shouldBe` e3
    Set.toList (c Vector.! 4) `shouldBe` e4
    Set.toList (c Vector.! 5) `shouldBe` e5
    Set.toList (c Vector.! 6) `shouldBe` e6

  it "should build the LALR parsing table" $ do
    -- Dragon Book (2nd edition, fr), page 247, figure 4.43
    let r0 = Rule "S" [NonTerm "C", NonTerm "C", Empty]
    let r1 = Rule "C" [Term 'c', NonTerm "C", Empty]
    let r2 = Rule "C" [Term 'd', Empty]
    let rs = ruleSet [Rule "__start" [NonTerm "S", Empty], r0, r1, r2]
    let fs = firstSet rs
    let c = collection rs fs :: LrCollection LalrItem
    case table c of
      Left err -> show err `shouldBe` "not an error"
      Right t -> do
        Map.size t `shouldBe` 18
        action t 0 (Term 'c') `shouldBe` LrShift 1
        action t 0 (Term 'd') `shouldBe` LrShift 2
        action t 0 (NonTerm "S") `shouldBe` LrGoto 4
        action t 0 (NonTerm "C") `shouldBe` LrGoto 3
        action t 1 (Term 'c') `shouldBe` LrShift 1
        action t 1 (Term 'd') `shouldBe` LrShift 2
        action t 1 (NonTerm "C") `shouldBe` LrGoto 5
        action t 2 (Term 'c') `shouldBe` LrReduce r2
        action t 2 (Term 'd') `shouldBe` LrReduce r2
        action t 2 Empty `shouldBe` LrReduce r2
        action t 3 (Term 'c') `shouldBe` LrShift 1
        action t 3 (Term 'd') `shouldBe` LrShift 2
        action t 3 (NonTerm "C") `shouldBe` LrGoto 6
        action t 4 Empty `shouldBe` LrAccept
        action t 5 (Term 'c') `shouldBe` LrReduce r1
        action t 5 (Term 'd') `shouldBe` LrReduce r1
        action t 5 Empty `shouldBe` LrReduce r1
        action t 6 Empty `shouldBe` LrReduce r0

  it "should detect conflicts during the table generation" $ do
    let r0 = Rule "__start" [NonTerm "S", Empty]
    let r1 = Rule "S" [Term 'a', NonTerm "A", Term 'd', Empty]
    let r2 = Rule "S" [Term 'b', NonTerm "B", Term 'd', Empty]
    let r3 = Rule "S" [Term 'a', NonTerm "B", Term 'e', Empty]
    let r4 = Rule "S" [Term 'b', NonTerm "A", Term 'e', Empty]
    let r5 = Rule "A" [Term 'c', Empty]
    let r6 = Rule "B" [Term 'c', Empty]
    let rs = ruleSet [r0, r1, r2, r3, r4, r5, r6]
    let fs = firstSet rs
    let c = collection rs fs :: LrCollection LalrItem
    let e = ["(4,'e') conflict: reduce B -> 'c',$ with reduce A -> 'c',$",
             "(4,'d') conflict: reduce B -> 'c',$ with reduce A -> 'c',$"]
    case table c of
      Left err -> err `shouldBe` e
      Right t  -> show t `shouldBe` "an error"
