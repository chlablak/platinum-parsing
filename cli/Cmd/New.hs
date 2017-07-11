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

  Log.info $ "create project into directory: " ++ projectName args
  Project.new $ projectName args

  -- End
  Log.popTag
  return ()
