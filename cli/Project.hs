{-|
Module      : Project
Description : Project system for PP ('pp.yaml' file)
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Project
    ( Project(..)
    , new
    ) where

import           Control.Monad
import           Data.Maybe
import           Data.Yaml        ((.:), (.=))
import qualified Data.Yaml        as Y
import qualified Log
import           System.Directory
import           System.IO

-- |Project file
pFile :: String -> FilePath
pFile d = d ++ "/pp.yaml"

-- |New project
new :: String -> Log.Logger
new name = do
  Log.io $ createDirectory name
  Log.io $ writeFile (name ++ "/grammar.ebnf") "(* Here comes the grammar *)"
  let p = Project name "0.0.0" "A short description" ["grammar.ebnf"] [] True
  setProject p

-- |Project configuration
data Project = Project
  { projectName        :: String
  , projectVersion     :: String
  , projectDescription :: String
  , projectGrammars    :: [String]
  , projectTemplates   :: [String]
  , projectUseWork     :: Bool
  }
  | NoProject
    deriving (Eq, Show)

instance Y.FromJSON Project where
  parseJSON (Y.Object v) = Project <$> v .: "name"
                                   <*> v .: "version"
                                   <*> v .: "description"
                                   <*> v .: "grammars"
                                   <*> v .: "templates"
                                   <*> v .: "use-work"
instance Y.ToJSON Project where
  toJSON v = Y.object [ "name" .= projectName v
                      , "version" .= projectVersion v
                      , "description" .= projectDescription v
                      , "grammars" .= projectGrammars v
                      , "templates" .= projectTemplates v
                      , "use-work" .= projectUseWork v]

-- |Get project config
getProject :: Log.LoggerIO Project
getProject = do
  p <- Log.io $ Y.decodeFile $ pFile "."
  return $ fromMaybe NoProject p

-- |Set project config
setProject :: Project -> Log.Logger
setProject p = do
  e <- Log.io $ doesFileExist $ pFile "."
  let f = if e then "." else projectName p
  Log.io $ Y.encodeFile (pFile f) p
