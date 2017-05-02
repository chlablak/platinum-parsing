module PPTest.Builders.Lr1 (specs) where

import qualified Data.Set        as Set
import qualified Data.Vector     as Vector
import           PP
import           PP.Builders.Lr1
import           Test.Hspec

specs = describe "PPTest.Builders.Lr1" $

  it "should build the LR(1) items set collection" $ do
    let rs = ruleSet [Rule "__start" [NonTerm "S", Empty],
                      Rule "S" [NonTerm "C", NonTerm "C", Empty],
                      Rule "C" [Term 'c', NonTerm "C", Empty],
                      Rule "C" [Term 'd', Empty]]
    let c = collection rs :: LrCollection Lr1Item
    let e0 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'c'),
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'd'),
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 (Term 'c'),
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 (Term 'd'),
              Lr1Item (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 0 Empty,
              Lr1Item (Rule "__start" [NonTerm "S",Empty]) 0 Empty]
    let e1 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'c'),
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 (Term 'd'),
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 (Term 'c'),
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 (Term 'd'),
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 (Term 'c'),
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 (Term 'd')]
    let e2 = [Lr1Item (Rule "C" [Term 'd',Empty]) 1 (Term 'c'),
              Lr1Item (Rule "C" [Term 'd',Empty]) 1 (Term 'd')]
    let e3 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 Empty,
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 Empty,
              Lr1Item (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 1 Empty]
    let e4 = [Lr1Item (Rule "__start" [NonTerm "S",Empty]) 1 Empty]
    let e5 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 (Term 'c'),
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 (Term 'd')]
    let e6 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 0 Empty,
              Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 1 Empty,
              Lr1Item (Rule "C" [Term 'd',Empty]) 0 Empty]
    let e7 = [Lr1Item (Rule "C" [Term 'd',Empty]) 1 Empty]
    let e8 = [Lr1Item (Rule "S" [NonTerm "C",NonTerm "C",Empty]) 2 Empty]
    let e9 = [Lr1Item (Rule "C" [Term 'c',NonTerm "C",Empty]) 2 Empty]
    Vector.length c `shouldBe` 10
    Set.toList (c Vector.! 0) `shouldBe` e0
    Set.toList (c Vector.! 1) `shouldBe` e1
    Set.toList (c Vector.! 2) `shouldBe` e2
    Set.toList (c Vector.! 3) `shouldBe` e3
    Set.toList (c Vector.! 4) `shouldBe` e4
    Set.toList (c Vector.! 5) `shouldBe` e5
    Set.toList (c Vector.! 6) `shouldBe` e6
    Set.toList (c Vector.! 7) `shouldBe` e7
    Set.toList (c Vector.! 8) `shouldBe` e8
    Set.toList (c Vector.! 9) `shouldBe` e9
