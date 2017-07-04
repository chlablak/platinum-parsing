module Main where

import qualified PPTest.Builders.Dfa
import qualified PPTest.Builders.Lalr
import qualified PPTest.Builders.Lr1
import qualified PPTest.Builders.Nfa
import qualified PPTest.Grammars.Ebnf
import qualified PPTest.Grammars.Lexical
import qualified PPTest.Grammars.LexicalHelper
import qualified PPTest.Lexers.Dfa
import qualified PPTest.Other.LexerDfaParserLr
import qualified PPTest.Parsers.Lr
import qualified PPTest.Rule
import qualified PPTest.Template
import qualified PPTest.Templates.Dfa
import qualified PPTest.Templates.Lr
import           Test.Hspec

main :: IO ()
main = hspec $
  describe "PPTest" $ do
    PPTest.Grammars.Ebnf.specs
    PPTest.Rule.specs
    PPTest.Builders.Lr1.specs
    PPTest.Builders.Lalr.specs
    PPTest.Parsers.Lr.specs
    PPTest.Templates.Lr.specs
    PPTest.Grammars.LexicalHelper.specs
    PPTest.Grammars.Lexical.specs
    PPTest.Builders.Nfa.specs
    PPTest.Builders.Dfa.specs
    PPTest.Lexers.Dfa.specs
    PPTest.Other.LexerDfaParserLr.specs
    PPTest.Templates.Dfa.specs
    PPTest.Template.specs
