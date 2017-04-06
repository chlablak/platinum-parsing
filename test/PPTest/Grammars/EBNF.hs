module PPTest.Grammars.EBNF (tests) where

import qualified PP
import           PP.Grammars.EBNF
import           Test.HUnit

tests = test ["equals" ~: generate (@?=) equals ]

generate :: (Syntax -> Syntax -> t) -> [(String, Syntax)] -> [t]
generate _ []         = []
generate a ((i,e):xs) = a (parse i) e : generate a xs
  where
    parse i = let Right x = PP.parse i in x

equals = [
    ("a=b;", Syntax [
      SyntaxRule (MetaIdentifier "a") (DefinitionsList [SingleDefinition [
        Term (Factor Nothing (PrimaryMetaIdentifier (MetaIdentifier "b"))) Nothing
    ]])])
  ]
