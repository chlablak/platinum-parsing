{-|
Module      : Main
Description : Command Line Interface for Platinum Parsing
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable

This module is the entry point for the pp-cli tool.
For more informations about this tool, please look at:
  https://github.com/chlablak/platinum-parsing
-}

module Main where

import           Args
import qualified Cmd.Ebnf
import qualified Cmd.Lalr
import           Control.Monad       (void)
import           Control.Monad.State
import           Data.Semigroup      ((<>))
import qualified Log
import           Options.Applicative

main :: IO ()
main = do
  args <- execParser opts
  void $ execStateT (dispatch args) Log.getLog
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> progDesc "Platinum Parsing CLI"
      <> header "tools for helping PP projects" )

-- |Dispatch arguments to commands
dispatch :: Args -> Log.Logger
dispatch args@(Args (CommonArgs l) _) = do
  Log.start l "pp"
  Log.autoFlush True
  Log.info "starting..."
  Log.info $ "verbosity: " ++ show l
  dispatch' args
  where
    dispatch' :: Args -> Log.Logger
    dispatch' a@(Args _ (EbnfCmd _)) = Cmd.Ebnf.dispatch a
    dispatch' a@(Args _ (LalrCmd _)) = Cmd.Lalr.dispatch a

-- |Arguments
args :: Parser Args
args = Args <$> commonArgs <*> commandArgs

-- |Common arguments
commonArgs :: Parser CommonArgs
commonArgs = CommonArgs
  <$> option auto ( long "verbosity"
    <> short 'v'
    <> metavar "LEVEL"
    <> value 30
    <> help "Set verbosity level" )

-- |Commands arguments
commandArgs :: Parser CommandArgs
commandArgs = hsubparser
  ( command "ebnf" (info Cmd.Ebnf.commandArgs (progDesc "Manipulate EBNF grammars"))
  <> command "lalr" (info Cmd.Lalr.commandArgs (progDesc "Generate LALR parsing table")))
