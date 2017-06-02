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
import           Control.Monad       (when)
import qualified Data.Map.Strict     as Map
import           Data.Semigroup      ((<>))
import qualified Data.Set            as Set
import qualified Data.Vector         as Vector
import qualified Log
import           Options.Applicative
import qualified PP
import qualified PP.Builders.Lalr    as Builder
import qualified PP.Grammars.Ebnf    as Grammar
import qualified PP.Parsers.Lr       as Parser
import qualified PP.Templates.Lr     as Template

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

-- |Command dispatch
dispatch :: Args -> Log.Logger
dispatch (Args _ (LalrCmd args)) = do
  Log.pushTag "lalr"
  input <- Log.io $ readFile $ lalrFile args
  case PP.parseAst input :: (PP.To Grammar.Syntax) of
    Left err -> do
      Log.err $ "error in file '" ++ lalrFile args ++ "':"
      Log.err $ show err
      Log.abort
    Right ast -> do
      r <- Log.io $ PP.rules' ast
      case r of
        Left err ->  do
          Log.err $ "cannot make rules: " ++ err
          Log.abort
        Right rules ->
          case PP.extend rules of
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
                let c = PP.collection rs fs :: PP.LrCollection Builder.LalrItem

                -- Flag '--collection'
                when (showCollection args) $
                  printCollection c

                -- Flag '--set'
                when (showSetI args /= (-1)) $
                  printSet (showSetI args) $ c Vector.! showSetI args

                let t = PP.table c
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

                    -- Flag '--test-with'
                    when (testWith args /= "") $ do
                      source <- Log.io $ readFile $ testWith args
                      let cfg = PP.parse' t $ PP.config t source :: [Parser.LrConfig]
                      printCfg cfg

                    -- Flag '--template'
                    when (template args /= "") $ do
                      te <- Log.io $ readFile $ template args
                      let compiled = PP.compile (Template.context t) te
                      Log.info "compiled template:"
                      Log.out compiled

  -- End
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
    printCfg' (Parser.LrConfig c _ a i) = do
      Log.info $ "after " ++ show c ++ " iterations: "
      case a of
        PP.LrAccept -> Log.info "input accepted"
        _           -> Log.err $ "error at " ++ show (take 20 i)
