{-|
Module      : Cmd.New
Description : CLI for the `pp new` command
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Cmd.New
    ( commandArgs
    , dispatch
    ) where

import           Args
import           Control.Monad       (when)
import           Data.Semigroup      ((<>))
import qualified Log
import           Options.Applicative
import qualified Project
import           System.Directory
import           System.IO

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = NewCmd <$> newArgs
  where
    newArgs = NewArgs
      <$> strOption ( long "name"
        <> short 'n'
        <> metavar "NAME"
        <> help "Project name" )

-- |Command dispatch
dispatch :: Args -> Log.Logger
dispatch (Args _ (NewCmd args)) = do
  Log.pushTag "new"

  let name = projectName args
  Log.info $ "create project into directory: " ++ name
  Log.io $ createDirectory name
  Log.io $ writeFile (name ++ "/grammar.ebnf") "(* Here comes the grammar *)"
  Log.io $ writeFile (name ++ "/.gitignore") ".pp-work/"
  let p = Project.Project name "0.0.0" "A short description" ["grammar.ebnf"] [] True []
  Project.set p

  -- End
  Log.popTag
  return ()
