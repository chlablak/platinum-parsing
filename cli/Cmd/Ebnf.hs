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

-- |Command dispatch
dispatch :: Args -> IO ()
dispatch (Args (CommonArgs verbose) (EbnfCmd (EbnfArgs file minify rules first))) = do

  -- Compute common things
  input <- readFile file
  case PP.parse input :: (PP.To Ebnf.Syntax) of
    Left err -> do
      putStrLn $ "error in file '" ++ file ++ "':"
      print err
    Right ast -> do
      -- Flag `--minify`
      when minify $ do
        Log.msg verbose 0 "EBNF" "minified:"
        putStrLn (PP.stringify ast)

      -- Flag `--rules`
      when rules $ do
        let r = PP.rules ast
        Log.msg verbose 0 "EBNF" "rules:"
        mapM_ print r

      -- Flag `--first`
      when first $ do
        let r = PP.rules ast
        let rs = PP.ruleSet r
        let fs = PP.firstSet rs
        Log.msg verbose 0 "EBNF" "first set:"
        mapM_ print $ Map.toList fs

  -- End
  return ()
