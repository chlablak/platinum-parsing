{-|
Module      : Cmd.Lalr
Description : CLI for the `pp lalr` command
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Cmd.Lalr
    ( commandArgs
    , dispatch
    ) where

import           Args
import           Control.Monad              (when)
import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.Map.Strict            as Map
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as Set
import qualified Data.Vector                as Vector
import qualified Log
import           Options.Applicative
import qualified PP
import qualified PP.Builders.Lalr           as Builder
import qualified PP.Grammars.Ebnf           as Grammar
import qualified PP.Lexers.Dfa              as Lexer
import qualified PP.Parsers.Lr              as Parser
import qualified PP.Templates.Dfa           as DfaTemplate
import qualified PP.Templates.Lr            as LrTemplate
import qualified Work

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = LalrCmd <$> lalrArgs
  where
    lalrArgs = LalrArgs
      <$> strOption ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> help "Input file" )
      <*> switch ( long "collection"
        <> help "Print the items sets collection" )
      <*> option auto ( long "set"
        <> metavar "I"
        <> value (-1)
        <> help "Print a specific items set" )
      <*> switch ( long "table"
        <> help "Print the LALR parsing table" )
      <*> strOption ( long "test-with"
        <> short 't'
        <> metavar "FILENAME"
        <> value ""
        <> help "Test the table on a source file" )
      <*> strOption ( long "template"
        <> metavar "FILENAME"
        <> value ""
        <> help "Specify a template file to use" )
      <*> switch ( long "dfa"
        <> help "Print the DFA" )
      <*> switch ( long "ast"
        <> help "Print the parsed AST (with --test-with)" )

-- |Command dispatch
dispatch :: Args -> Log.Logger
dispatch (Args cargs (LalrCmd args)) = do
  Log.pushTag "lalr"
  input <- Log.io $ readFile $ lalrFile args
  case PP.parseAst input :: (PP.To Grammar.Syntax) of
    Left err -> do
      Log.err $ "error in file '" ++ lalrFile args ++ "':"
      Log.err $ show err
      Log.abort
    Right ast -> do
      r <- Log.io $ PP.rules' $ PP.lexify ast
      case r of
        Left err ->  do
          Log.err $ "cannot make rules: " ++ err
          Log.abort
        Right rules -> do
          let (prs, lrs) = PP.separate rules
          case PP.extend prs of
            Left err -> do
              Log.err "cannot extend the input grammar:"
              Log.err err
              Log.abort
            Right g' -> do
              let rs = PP.ruleSet g'
              let (errors, warnings) = PP.check rs
              mapM_ Log.warn warnings
              if errors /= [] then do
                Log.err "errors found in rules:"
                mapM_ Log.err errors
                Log.abort
              else do
                let fs = PP.firstSet rs
                Log.pushTask "compute collection and table"
                c <- Work.reuse (useWork cargs)
                                (lalrFile args)
                                "collection"
                                (Log.io $ return $ PP.collection rs fs)
                     :: Log.LoggerIO (PP.LrCollection Builder.LalrItem)

                -- Flag '--collection'
                when (showCollection args) $
                  printCollection c

                -- Flag '--set'
                when (showSetI args /= (-1)) $
                  printSet (showSetI args) $ c Vector.! showSetI args

                t <- Work.reuse (useWork cargs)
                                (lalrFile args)
                                "table"
                                (Log.io $ return $ PP.table c)
                case t of
                  Left err -> do
                    Log.popTask
                    Log.err "grammar is not LALR:"
                    mapM_ Log.err err
                    Log.abort
                  Right t -> do
                    Log.popTask

                    -- Flag '--table'
                    when (showTable args) $ do
                      Log.info "table:"
                      printTable t

                    Log.pushTask "compute DFA"
                    dfa' <- Work.reuse (useWork cargs)
                                       (lalrFile args)
                                       "dfa"
                                       (Log.io $ Lexer.createDfa' lrs)
                    case dfa' of
                      Left err -> do
                        Log.popTask
                        Log.err $ "cannot create DFA: " ++ err
                        Log.abort
                      Right dfa -> do
                        Log.popTask

                        -- Flag `--dfa`
                        when (showDfa args) $
                          printDfa dfa

                        -- Flag '--test-with'
                        when (testWith args /= "") $ do
                          source <- Log.io $ readFile $ testWith args
                          let lconfig = Lexer.dfaConfig source dfa
                          let tokens = PP.output $ PP.consume lconfig
                          let cfg = PP.parse' t $ PP.config t tokens :: [Parser.LrConfig]

                          -- Flag `--ast`
                          when (showAst args) $ do
                            Log.info "parsed AST:"
                            Log.out $ Parser.prettyAst $ Parser.lrAst $ head cfg

                          printCfg cfg

                        -- Flag '--template'
                        when (template args /= "") $ do
                          te <- Log.io $ readFile $ template args
                          let c1 = LrTemplate.context t
                          let c2 = DfaTemplate.context dfa
                          let compiled = PP.compile (PP.mergeContext c1 c2) te
                          Log.info "compiled template:"
                          Log.out compiled

  -- End
  Log.popTag
  return ()

-- |Pretty print for collection
printCollection :: PP.LrCollection Builder.LalrItem -> Log.Logger
printCollection c = do
  Log.info "collection:"
  Vector.imapM_ printSet c

-- |Pretty print for set
printSet :: Int -> PP.LrSet Builder.LalrItem -> Log.Logger
printSet i is = do
  Log.info $ "items set " ++ show i ++ ":"
  mapM_ (Log.out . show) $ Set.toList is

-- |Pretty print for table
printTable :: PP.LrTable -> Log.Logger
printTable = Log.out . Map.showTree

-- |Pretty print for configuration
printCfg :: [Parser.LrConfig] -> Log.Logger
printCfg = printCfg' . head
  where
    printCfg' (Parser.LrConfig c _ a i _) = do
      Log.out $ "after " ++ show c ++ " iterations: "
      case a of
        PP.LrAccept -> Log.out "input accepted"
        _           -> Log.out $ "error at " ++ show (take 20 (str i))
    str = concatMap (\(PP.OToken2 v _) -> v)

-- |Pretty print for DFA
printDfa :: PP.DfaGraph -> Log.Logger
printDfa = Log.out . Gr.prettify
