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
import           Data.Semigroup      ((<>))
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

-- |Command dispatch
dispatch :: Args -> IO ()
dispatch (Args (CommonArgs verbose) (EbnfCmd (EbnfArgs file minify rules))) = do

  -- Compute common things
  input <- readFile file
  case PP.parse input :: (PP.To Ebnf.Syntax) of
    Left err -> do
      putStrLn $ "error in file '" ++ file ++ "':"
      print err
    Right ast -> do
      -- Flag `--minify`
      when minify $ putStrLn (PP.stringify ast)

      -- Flag `--rules`
      when rules $ do
        let r = PP.rules ast
        mapM_ print r

  -- End
  return ()
