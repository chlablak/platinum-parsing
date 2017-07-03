module PPTest.Other.LexerDfaParserLr (specs) where

import qualified Data.Map.Strict  as Map
import           PP
import qualified PP.Builders.Lalr as Lalr
import qualified PP.Grammars.Ebnf as Ebnf
import qualified PP.Lexers.Dfa    as Dfa
import qualified PP.Parsers.Lr    as Lr
import           Test.Hspec

import qualified PP.Builders.Nfa  as Nfa

-- Grammar
g = "expr=number,{binop,number};\n\
    \binop%=\"-|[+]\";\n\
    \number%=digit,\"+\";\n\
    \digit%=\"[0-9]\";"
Right ast = parseAst g :: To Ebnf.Syntax

specs = describe "PPTest.Other.LexerDfaParserLr" $ do

  it "should be able to extract rules from the AST" $ do
    let e = [Rule "binop" [RegEx "-|[+]",Empty],
             Rule "digit" [RegEx "[0-9]",Empty],
             Rule "expr" [NonTerm "number",NonTerm "{<binop>,<number>}",Empty],
             Rule "number" [NonTerm "digit",RegEx "+",Empty],
             Rule "{<binop>,<number>}" [NonTerm "binop",NonTerm "number",NonTerm "{<binop>,<number>}",Empty],
             Rule "{<binop>,<number>}" [Empty]]
    rules (lexify ast) `shouldBe` e

  it "should be able to separate parsing and lexing rules" $ do
    let e = ([Rule "expr" [TermToken "number",NonTerm "{<binop>,<number>}",Empty],
              Rule "{<binop>,<number>}" [TermToken "binop",TermToken "number",NonTerm "{<binop>,<number>}",Empty],
              Rule "{<binop>,<number>}" [Empty]],
             [Rule "binop" [RegEx "-|[+]",Empty],
              Rule "digit" [RegEx "[0-9]",Empty],
              Rule "number" [TermToken "digit",RegEx "+",Empty]])
    separate (rules $ lexify ast) `shouldBe` e

  it "should be able to create a LALR table" $ do
    let Right prs = extend $ fst $ separate $ rules $ lexify ast
    let rs = ruleSet prs
    let c = collection rs (firstSet rs) :: LrCollection Lalr.LalrItem
    let e = [((0,NonTerm "expr"),LrGoto 1),
             ((0,TermToken "number"),LrShift 2),
             ((1,Empty),LrAccept),
             ((2,NonTerm "{<binop>,<number>}"),LrGoto 3),
             ((2,TermToken "binop"),LrShift 4),
             ((2,Empty),LrReduce $ Rule "{<binop>,<number>}" [Empty]),
             ((3,Empty),LrReduce $ Rule "expr" [TermToken "number",NonTerm "{<binop>,<number>}",Empty]),
             ((4,TermToken "number"),LrShift 5),
             ((5,NonTerm "{<binop>,<number>}"),LrGoto 6),
             ((5,TermToken "binop"),LrShift 4),
             ((5,Empty),LrReduce $ Rule "{<binop>,<number>}" [Empty]),
             ((6,Empty),LrReduce $ Rule "{<binop>,<number>}" [TermToken "binop",TermToken "number",NonTerm "{<binop>,<number>}",Empty])]
    case table c of
      Left err -> show err `shouldNotBe` "an error"
      Right t  -> Map.toList t `shouldBe` e

  it "should be able to create the correct tokens" $ do
    let i = "123-456+789"
    let (_, lrs) = separate $ rules $ lexify ast
    let lconfig = Dfa.dfaConfig i $ Dfa.createDfa lrs
    let e = [OToken2 "123" "number",
             OToken2 "-" "binop",
             OToken2 "456" "number",
             OToken2 "+" "binop",
             OToken2 "789" "number"]
    output (consume lconfig) `shouldBe` e

  it "should be able to use a lexer and a parser in sequence" $ do
    let i = "123-456+789"
    let (prs, lrs) = separate $ rules $ lexify ast
    let rs = ruleSet $ let Right x = extend prs in x
    let c = collection rs (firstSet rs) :: LrCollection Lalr.LalrItem
    let Right t = table c
    let dfa = Dfa.createDfa lrs
    let lconfig = Dfa.dfaConfig i dfa
    let tokens = output $ consume lconfig
    let pconfig = config t tokens
    Lr.lrAction (parse t pconfig) `shouldBe` LrAccept
