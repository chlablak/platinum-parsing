module PPTest.Grammars.EBNF (specs) where

import           Data.Either
import           PP
import           PP.Grammars.EBNF
import           System.IO
import           Test.Hspec

specs = describe "PPTest.Grammars.EBNF" $ do

  it "should detect a simple syntax error" $
    case parse "a = b" :: To Syntax of
      Left e  -> show e `shouldNotBe` ""
      Right o -> stringify o `shouldBe` "an error"

  it "should detect bad enclosing" $
    case parse "a = (b | [c - {d})" :: To Syntax of
      Left e  -> show e `shouldNotBe` ""
      Right o -> stringify o `shouldBe` "an error"

  it "should parse a simple correct input" $
    case parse "a = b;" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>;"

  it "should parse a more complex correct input" $
    case parse "a = b, (c | d);\ne = 4 * [f];\nh = i | j;" :: To Syntax of
      Left e -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>,(<c>|<d>);\n<e>=4*[<f>];\n<h>=<i>|<j>;"

  it "should parse terminal string" $
    case parse "a = \"h 'w\"; b = \"h \\\"w\";" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=\"h 'w\";\n<b>=\"h \\\"w\";"

  it "should parse complex meta identifiers" $
    case parse "a a = b; a = b b;" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a a>=<b>;\n<a>=<b b>;"

  it "should ignore comments" $
    case parse "(* 1 *) a = b; (* 2 *) c = d; (* 3 *)" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>;\n<c>=<d>;"

  it "should deal with any white spaces" $
    case parse " \t\n a \n  = \tb  \n\t|  c  ; \t " :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>|<c>;"

  it "should parse the complete EBNF grammar" $ do
    g <- readFile "doc/grammars/ebnf.ebnf"
    m <- readFile "doc/grammars/ebnf.min.ebnf"
    case parse g :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o ++ "\n" `shouldBe` m
