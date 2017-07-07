module PPTest.Grammars.Lexical (specs) where

import           PP
import           PP.Grammars.Lexical
import           Test.Hspec

specs = describe "PPTest.Grammars.Lexical" $ do

  it "should parse a regular expression (any)" $
    case parseAst "." :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "."

  it "should parse a regular expression (value)" $
    case parseAst "a" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a"

  it "should parse a regular expression (class interval)" $
    case parseAst "[a-z]" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "[a-z]"

  it "should parse a regular expression (class)" $
    case parseAst "[a-z0-9.-]" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "[a-z0-9.-]"

  it "should parse a regular expression (group)" $
    case parseAst "(a)" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "(a)"

  it "should parse a regular expression (option)" $
    case parseAst "a?" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a?"

  it "should parse a regular expression (many1)" $
    case parseAst "a+" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a+"

  it "should parse a regular expression (many0)" $
    case parseAst "a*" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a*"

  it "should parse a regular expression (choice)" $
    case parseAst "abc" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "abc"

  it "should parse a regular expression (regexpr)" $
    case parseAst "ab|cd" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "ab|cd"

  it "should parse a complex regular expression" $
    case parseAst "(a*b)?|[a-z]+(a|[b-d])?|(a|(b|c))de|.|" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "(a*b)?|[a-z]+(a|[b-d])?|(a|(b|c))de|.|"

  it "should parse all meta symbols into class" $
    case parseAst "[[][|][*][+][?][(][(][]][.]" :: To RegExpr of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "[[][|][*][+][?][(][(][]][.]"
