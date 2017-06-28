module PPTest.Grammars.Ebnf (specs) where

import           Data.Either
import           PP
import           PP.Grammars.Ebnf
import           System.IO
import           Test.Hspec

specs = describe "PPTest.Grammars.Ebnf" $ do

  it "should detect a simple syntax error" $
    case parseAst "a = b" :: To Syntax of
      Left e  -> show e `shouldNotBe` ""
      Right o -> stringify o `shouldBe` "an error"

  it "should detect bad enclosing" $
    case parseAst "a = (b | [c - {d})" :: To Syntax of
      Left e  -> show e `shouldNotBe` ""
      Right o -> stringify o `shouldBe` "an error"

  it "should parseAst a simple correct input" $
    case parseAst "a = b;" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>;"

  it "should parseAst a more complex correct input" $
    case parseAst "a = b, (c | d);\ne = 4 * [f];\nh = i | j;" :: To Syntax of
      Left e -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>,(<c>|<d>);\n<e>=4*[<f>];\n<h>=<i>|<j>;"

  it "should parseAst terminal string" $
    case parseAst "a = \"h 'w\"; b = \"h \\\"w\";" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=\"h 'w\";\n<b>=\"h \\\"w\";"

  it "should parseAst complex meta identifiers" $
    case parseAst "a a = b; a = b b; <a>=b; a=<b>;" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a a>=<b>;\n<a>=<b b>;\n<a>=<b>;\n<a>=<b>;"

  it "should ignore comments" $
    case parseAst "(* 1 *) a = b; (* 2 *) c = d; (* 3 *)" :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>;\n<c>=<d>;"

  it "should deal with any white spaces" $
    case parseAst " \t\n a \n  = \tb  \n\t|  c  ; \t " :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o `shouldBe` "<a>=<b>|<c>;"

  it "should parseAst the complete EBNF grammar" $ do
    g <- readFile "doc/grammars/ebnf.ebnf"
    m <- readFile "doc/grammars/ebnf.min.ebnf"
    case parseAst g :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o ++ "\n" `shouldBe` m

  it "should parseAst the complete minified EBNF grammar" $ do
    g <- readFile "doc/grammars/ebnf.min.ebnf"
    m <- readFile "doc/grammars/ebnf.min.ebnf"
    case parseAst g :: To Syntax of
      Left e  -> show e `shouldBe` "not an error"
      Right o -> stringify o ++ "\n" `shouldBe` m

  it "should handle multiple parseAst and stringify" $
    case parseAst "a = b;" :: To Syntax of
      Left e1  -> show e1 `shouldBe` "not an error (e1)"
      Right o1 -> let s1 = stringify o1 in
        case parseAst s1 :: To Syntax of
          Left e2  -> show e2 `shouldBe` "not an error (e2)"
          Right o2 -> stringify o2 `shouldBe` "<a>=<b>;"

  it "should handle translation to canonical rules (meta identifiers)" $
    let Right ast = parseAst "a = b;" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", PP.Empty]]

  it "should handle translation to canonical rules (alternatives)" $
    let Right ast = parseAst "a = b | c;" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", PP.Empty],
                            Rule "a" [NonTerm "c", PP.Empty]]

  it "should handle translation to canonical rules (terminal string)" $
    let Right ast = parseAst "a = \"hi\";" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [PP.Term 'h', PP.Term 'i', PP.Empty]]

  it "should handle translation to canonical rules (optional sequences)" $
    let Right ast = parseAst "a = [b];" :: To Syntax in
      rules ast `shouldBe` [Rule "[<b>]" [NonTerm "b", PP.Empty],
                            Rule "[<b>]" [PP.Empty],
                            Rule "a" [NonTerm "[<b>]", PP.Empty]]

  it "should handle translation to canonical rules (repeated sequences, left)" $
    let Right ast = parseAst "a = {b}, c;" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "{<b>}", NonTerm "c", PP.Empty],
                            Rule "{<b>}" [NonTerm "b", NonTerm "{<b>}", PP.Empty],
                            Rule "{<b>}" [PP.Empty]]

  it "should handle translation to canonical rules (repeated sequences, right)"$
    let Right ast = parseAst "a = c, {b};" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "c", NonTerm "{<b>}", PP.Empty],
                            Rule "{<b>}" [NonTerm "b", NonTerm "{<b>}", PP.Empty],
                            Rule "{<b>}" [PP.Empty]]

  it "should handle translation to canonical rules (grouped sequences)" $
    let Right ast = parseAst "a = (b | c), d;" :: To Syntax in
      rules ast `shouldBe` [Rule "(<b>|<c>)" [NonTerm "b", PP.Empty],
                            Rule "(<b>|<c>)" [NonTerm "c", PP.Empty],
                            Rule "a" [NonTerm "(<b>|<c>)", NonTerm "d", PP.Empty]]

  it "should handle translation to canonical rules (factor)" $
    let Right ast = parseAst "a = 2 * b;" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", NonTerm "b", PP.Empty]]

  it "should handle translation to canonical rules (exception)" $
    let Right ast = parseAst "a = <b> - c;" :: To Syntax in
      pendingWith "exception not supported yet"

  it "should handle translation to canonical rules (complex rules)" $
    let Right ast = parseAst "a = [(b, c) | {d}], e | (f | g);" :: To Syntax in
      rules ast `shouldBe` [Rule "(<b>,<c>)" [NonTerm "b",NonTerm "c",PP.Empty],
                            Rule "(<f>|<g>)" [NonTerm "f",PP.Empty],
                            Rule "(<f>|<g>)" [NonTerm "g",PP.Empty],
                            Rule "[(<b>,<c>)|{<d>}]" [NonTerm "(<b>,<c>)",PP.Empty],
                            Rule "[(<b>,<c>)|{<d>}]" [NonTerm "{<d>}",PP.Empty],
                            Rule "[(<b>,<c>)|{<d>}]" [PP.Empty],
                            Rule "a" [NonTerm "(<f>|<g>)",PP.Empty],
                            Rule "a" [NonTerm "[(<b>,<c>)|{<d>}]",NonTerm "e",PP.Empty],
                            Rule "{<d>}" [NonTerm "d",NonTerm "{<d>}",PP.Empty],
                            Rule "{<d>}" [PP.Empty]]

  it "should handle lexical rules (PP.Grammars.LexicalHelper)" $
    let Right ast = parseAst "a = b; b %= c; c = d;" :: To Syntax in
      rules ast `shouldBe` [Rule "a" [NonTerm "b", PP.Empty],
                            Rule "b" [NonTerm "c", PP.Empty],
                            Rule "c" [NonTerm "d", PP.Empty]]

  it "should lexify correctly" $
    let Right ast = parseAst "d%=\"[0-9]\";\nn%=d,\"+\";\ns=\"ab\",n;" :: To Syntax in
      stringify (lexify ast) `shouldBe` "__token_0%=\"ab\";\nd%=\"[0-9]\";\nn%=d,\"+\";\n<s>=<__token_0>,<n>;"
