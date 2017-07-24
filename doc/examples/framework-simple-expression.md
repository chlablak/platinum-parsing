# Example: simple expression
This tutorial aim to provide a quick introduction of usage.

The objective is to parse a simple expression `23-34+87` and get the AST of it, using PP.

We will explore the use of the Framework.

You can find the same example but for the CLI at `doc/examples/cli-simple-expression.md`.

## Step 1: creating the project
For this tutorial we assume that you have `stack` installed.

Begin by creating a new Haskell project:

  ```console
  $ stack new expr
  $ cd expr/
  ```

Then open `expr.cabal` and add `platinum-parsing` as a build dependency. You should have something like:

  ```yaml
  executable expr-exe
    hs-source-dirs:      app
    main-is:             Main.hs
    ghc-options:         -threaded -rtsopts -with-rtsopts=-N
    build-depends:       base
                       , expr
                       , platinum-parsing
    default-language:    Haskell2010
  ```

Finally build the project to ensure everything is ok:

  ```console
  $ stack solver --update-config
  $ stack build
  $ stack exec -- expr-exe
  someFunc
  ```

## Step 2: writing the grammar
For this simple language, copy the following grammar in a file named `grammar.ebnf`:

  ```ebnf
  (* This production is a syntax rule *)
  (* It will be used by the parser *)
  Expr = Number, { BinOP, Number }, "\n" ;

  (* Those production are lexical rules (regular expression) *)
  (* They will be used by the lexer to produce tokens *)
  Number %= "[1-9][0-9]*" ;
  BinOP %= "[+-]" ;
  (* newline needed at the end *)
  ```

## Step 3: writing the source code
Now open the file `app/Main.hs` and edit it like this:

  ```haskell
  module Main where

  -- Usefull imports
  import qualified PP
  import qualified PP.Builders.Lalr as Builder
  import qualified PP.Grammars.Ebnf as Grammar
  import qualified PP.Lexers.Dfa    as Lexer
  import qualified PP.Parsers.Lr    as Parser

  -- Expression we want to parse
  expr :: String
  expr = "23-34+87\n"

  main :: IO ()
  main = do

    -- Read the grammar file
    grammar <- readFile "grammar.ebnf"

    -- Parse the grammar
    case PP.parseAst grammar :: (PP.To Grammar.Syntax) of
      Left err -> print err
      Right ast -> do

        -- Get canonical rules from the grammar AST
        let rules = PP.rules $ PP.lexify ast

        -- Separate production rules and lexical rules
        let (prs, lrs) = PP.separate rules

        -- Extend the rules to have an augmented grammar (g')
        case PP.extend prs of
          Left err -> print err
          Right g' -> do

            -- Put the rules into a RuleSet and compute first set
            let ruleSet = PP.ruleSet g'
            let firstSet = PP.firstSet ruleSet

            -- Compute the LALR items set collection
            let collection = PP.collection ruleSet firstSet :: (PP.LrCollection Builder.LalrItem)

            -- Compute the LALR table
            case PP.table collection of
              Left err -> print err
              Right table -> do

                -- Compute the DFA (for the lexer)
                let lrs' = PP.removeUnusedToken ruleSet $ PP.regexfy lrs
                let dfa = Lexer.createDfa lrs'

                -- Create the lexer configuration and run it to get tokens
                let lconfig = Lexer.dfaConfig expr dfa
                let tokens = PP.output $ PP.consume lconfig

                -- Create the parser config and run it to get the AST
                let pconfig = PP.config table tokens :: Parser.LrConfig
                let exprAst = Parser.lrAst $ PP.parse table pconfig

                -- Print the AST
                putStrLn $ Parser.prettyAst exprAst

    -- End
    return ()
  ```

For more details take a look at the hackage documentation.

## Step 4: building the executable
Finally we build the executable and run it:

  ```console
  $ stack build
  $ stack exec -- expr-exe
  |Expr
    |[OToken2 "23" "Number"]
    |{<BinOP>,<Number>}
      |[OToken2 "-" "BinOP"]
      |[OToken2 "34" "Number"]
      |{<BinOP>,<Number>}
        |[OToken2 "+" "BinOP"]
        |[OToken2 "87" "Number"]
        |{<BinOP>,<Number>}
    |[OToken2 "\n" "__token_\n"]
  ```

That's it !
