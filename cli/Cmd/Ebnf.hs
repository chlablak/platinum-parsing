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
      <$> strOption ( long "file" <> short 'f' <> metavar "FILENAME"
        <> help "Input file" )
      <*> switch ( long "minify"
        <> help "Print the minified grammar" )
      <*> switch ( long "rules"
        <> help "Print the obtained rules" )
      <*> switch ( long "first"
        <> help "Print the first set" )
      <*> switch ( long "check"
        <> help "Search for errors" )

-- |Command dispatch
dispatch :: Args -> IO ()
dispatch (Args (CommonArgs verbose)
               (EbnfCmd (EbnfArgs file minify rules first check))) = do

  input <- readFile file
  case PP.parseAst input :: (PP.To Ebnf.Syntax) of
    Left err -> do
      putStrLn $ "error in file '" ++ file ++ "':"
      print err
    Right ast -> do
      -- Flag `--minify`
      when minify $ do
        Log.msg verbose 0 "EBNF" "minified:"
        putStrLn (PP.stringify ast)

      r <- PP.rules' ast
      case r of
        Left err -> putStrLn $ "cannot make rules: " ++ err
        Right r ->
          case PP.extend r of
            Left err -> do
              putStrLn "cannot extend the input grammar:"
              print err
            Right g' -> do
              let rs = PP.ruleSet g'
              let fs = PP.firstSet rs

              -- Flag `--rules`
              when rules $ do
                Log.msg verbose 0 "EBNF" "rules:"
                mapM_ print g'

              -- Flag `--first`
              when first $ do
                Log.msg verbose 0 "EBNF" "first set:"
                mapM_ print $ Map.toList fs

              -- Flag `--check`
              when check $ do
                let (err, warn) = PP.check rs
                Log.msg verbose 0 "EBNF" "check:"
                Log.msg verbose 0 "EBNF" "errors:"
                mapM_ putStrLn err
                Log.msg verbose 0 "EBNF" "warnings:"
                mapM_ putStrLn warn

  -- End
  return ()
