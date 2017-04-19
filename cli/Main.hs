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
import           Data.Semigroup      ((<>))
import           Options.Applicative

main :: IO ()
main = dispatch =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> progDesc "Platinum Parsing CLI"
      <> header "tools for helping PP projects" )

-- |Dispatch arguments to commands
dispatch :: Args -> IO ()
dispatch a@(Args _ (EbnfCmd _)) = Cmd.Ebnf.dispatch a

-- |Arguments
args :: Parser Args
args = Args <$> commonArgs <*> commandArgs

-- |Common arguments
commonArgs :: Parser CommonArgs
commonArgs = CommonArgs
  <$> switch ( long "verbose" <> short 'v'
    <> help "Enable verbose mode" )

-- |Commands arguments
commandArgs :: Parser CommandArgs
commandArgs = hsubparser
  ( command "ebnf" (info Cmd.Ebnf.commandArgs (progDesc "Manipulate EBNF grammars")) )
