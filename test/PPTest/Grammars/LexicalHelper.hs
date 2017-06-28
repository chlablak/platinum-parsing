module PPTest.Grammars.LexicalHelper (specs) where

import           PP
import           PP.Grammars.LexicalHelper
import           Test.Hspec

specs = describe "PPTest.Grammars.LexicalHelper" $ do

  it "should detect a simple syntax error" $
    case parseAst "a %= b" :: To LexicalRule of
      Left e  -> show e `shouldNotBe` ""
      Right o -> stringify o `shouldBe` "an error"

  it "should parseAst a simple correct input" $
    case parseAst "a %= b;" :: To LexicalRule of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a%=b;"

  it "should parseAst a more complex correct input" $
    case parseAst "a %= b, \"c\", d;" :: To LexicalRule of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a%=b,\"c\",d;"

  it "should parseAst terminal string" $
    case parseAst "a %= \"h 'w\", \"h \\\"w\";" :: To LexicalRule of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a%=\"h 'w\",\"h \\\"w\";"

  it "should parseAst complex meta identifiers" $
    case parseAst "a a %= b b;" :: To LexicalRule of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a a%=b b;"

  it "should deal with any white spaces" $
    case parseAst " \t\n a \n  %= \tb  \n\t,  c  ; \t " :: To LexicalRule of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "a%=b,c;"

  it "should handle multiple parseAst and stringify" $
    case parseAst "a %= b, \"c\";" :: To LexicalRule of
      Left e1  -> show e1 `shouldBe` "not an error (e1)"
      Right o1 -> let s1 = stringify o1 in
        case parseAst s1 :: To LexicalRule of
          Left e2  -> show e2 `shouldBe` "not an error (e2)"
          Right o2 -> stringify o2 `shouldBe` "a%=b,\"c\";"

  it "should handle translation to canonical rules (lexical identifiers)" $
    let Right ast = parseAst "a %= b;" :: To LexicalRule in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", Empty]]

  it "should handle translation to canonical rules (lexical string)" $
    let Right ast = parseAst "a %= \"hi\";" :: To LexicalRule in
      rules ast `shouldBe` [Rule "a" [RegEx "hi", Empty]]

  it "should handle translation to canonical rules (complex rules)" $
    let Right ast = parseAst "a %= b, \"c\", d;" :: To LexicalRule in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", RegEx "c", NonTerm "d", Empty]]
