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
import qualified PP.Builders.Lalr    as Lalr
import qualified PP.Grammars.Ebnf    as Ebnf

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = LalrCmd <$> lalrArgs
  where
    lalrArgs = LalrArgs
      <$> strOption ( long "file" <> short 'f' <> metavar "FILENAME"
        <> help "Input grammar (EBNF)" )
      <*> switch ( long "collection"
        <> help "Print the items sets collection" )
      <*> option auto ( long "set"
        <> metavar "I"
        <> value (-1)
        <> help "Print a specific items set")
      <*> switch ( long "table"
        <> help "Print the LALR parsing table" )

-- |Command dispatch
dispatch :: Args -> IO ()
dispatch (Args (CommonArgs verbose) (LalrCmd (LalrArgs grammar collection set table))) = do

  -- Compute common things
  input <- readFile grammar
  case PP.parseAst input :: (PP.To Ebnf.Syntax) of
    Left err -> do
      putStrLn $ "error in file '" ++ grammar ++ "':"
      print err
    Right ast -> do
      rules <- Log.task verbose "compute rules" (\() -> PP.rules ast)
      case PP.extend rules of
        Left err -> do
          putStrLn "cannot extend the input grammar:"
          print err
        Right g' -> do
          let rs = PP.ruleSet g'
          let fs = PP.firstSet rs
          c <- Log.task verbose "compute collection" (\() ->
            PP.collection rs fs :: PP.LrCollection Lalr.LalrItem)

          -- Flag '--collection'
          when collection $ do
            Log.msg verbose 0 "LALR" "collection:"
            printCollection c

          -- Flag '--set'
          when (set /= (-1)) $
            printSet set $ c Vector.! set

          -- Flag '--table'
          when table $ do
            t <- Log.task verbose "compute table" (\() -> PP.table c)
            Log.msg verbose 0 "LALR" "table:"
            printTable t

  -- End
  return ()

-- |Pretty print for collection
printCollection :: PP.LrCollection Lalr.LalrItem -> IO ()
printCollection = Vector.imapM_ printCollection'
  where
    printCollection' i is = do
      printSet i is
      putStrLn ""

-- |Pretty print for set
printSet :: Int -> PP.LrSet Lalr.LalrItem -> IO()
printSet i is = do
  putStrLn $ "items set " ++ show i ++ ":"
  mapM_ print $ Set.toList is

-- |Pretty print for table
printTable :: PP.LrTable -> IO ()
printTable = putStrLn . Map.showTree
