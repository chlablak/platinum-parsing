{-|
Module      : Cmd.Ebnf
Description : CLI for the `pp ebnf` command
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Cmd.Ebnf
    ( commandArgs
    , dispatch
    ) where

import           Args
import           Control.Monad       (when)
import qualified Data.Map.Strict     as Map
import           Data.Semigroup      ((<>))
import qualified Log
import           Options.Applicative
import qualified PP
import qualified PP.Grammars.Ebnf    as Ebnf

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = EbnfCmd <$> ebnfArgs
  where
    ebnfArgs = EbnfArgs
      <$> strOption ( long "file"
        <> short 'f'
        <> metavar "FILENAME"
        <> help "Input file" )
      <*> switch ( long "minify"
        <> help "Print the minified grammar" )
      <*> switch ( long "rules"
        <> help "Print the obtained rules" )
      <*> switch ( long "first"
        <> help "Print the first set" )
      <*> switch ( long "check"
        <> help "Search for errors" )
      <*> switch ( long "lexical"
        <> help "Print lexical rules" )

-- |Command dispatch
dispatch :: Args -> Log.Logger
dispatch (Args _ (EbnfCmd args)) = do
  Log.pushTag "ebnf"
  input <- Log.io $ readFile $ ebnfFile args
  case PP.parseAst input :: (PP.To Ebnf.Syntax) of
    Left err -> do
      Log.err $ "error in file '" ++ ebnfFile args ++ "':"
      Log.err $ show err
      Log.abort
    Right ast -> do
      -- Flag `--minify`
      when (showMinified args) $ do
        Log.info "minified:"
        Log.out $ PP.stringify ast

      r <- Log.io $ PP.rules' $ PP.lexify ast
      case r of
        Left err -> do
          Log.err $ "cannot make rules: " ++ err
          Log.abort
        Right r -> do
          let (prs, lrs) = PP.separate r

          -- Flag `--lexical`
          when (showLexical args) $ do
            Log.info "lexical rules:"
            mapM_ (Log.out . show) lrs

          case PP.extend prs of
            Left err -> do
              Log.err "cannot extend the input grammar:"
              Log.err err
              Log.abort
            Right g' -> do
              let rs = PP.ruleSet g'
              let fs = PP.firstSet rs

              -- Flag `--rules`
              when (showRules args) $ do
                Log.info "rules:"
                mapM_ (Log.out . show) g'

              -- Flag `--first`
              when (showFirstSet args) $ do
                Log.info "first set:"
                mapM_ (Log.out . show) $ Map.toList fs

              -- Flag `--check`
              when (doCheck args) $ do
                let (err, warn) = PP.check rs
                Log.pushTag "check"
                Log.info "errors:"
                mapM_ Log.info err
                Log.info "warnings:"
                mapM_ Log.info warn

  -- End
  return ()
