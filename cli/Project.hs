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
    , ProjectTemplate(..)
    , new
    , get
    , set
    ) where

import qualified Args
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
  set p

-- |Project configuration
data Project = Project
  { projectName        :: String
  , projectVersion     :: String
  , projectDescription :: String
  , projectGrammars    :: [String]
  , projectTemplates   :: [ProjectTemplate]
  , projectUseWork     :: Bool
  }
  | NoProject
  | MalformedProject String
    deriving (Eq, Show)
data ProjectTemplate = ProjectTemplate
  { templateFile :: String
  , templateDst  :: String
  }
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

instance Y.FromJSON ProjectTemplate where
  parseJSON (Y.Object v) = ProjectTemplate <$> v .: "file"
                                           <*> v .: "destination"
instance Y.ToJSON ProjectTemplate where
  toJSON v = Y.object [ "file" .= templateFile v
                      , "destination" .= templateDst v]

-- |Get project config
get :: Log.LoggerIO Project
get = do
  let f = pFile "."
  e <- Log.io $ doesFileExist f
  if e then do
    p <- Log.io (Y.decodeFileEither f :: IO (Either Y.ParseException Project))
    case p of
      Left err -> return $ MalformedProject $ show err
      Right q  -> return q
  else
    return NoProject

-- |Set project config
set :: Project -> Log.Logger
set p = do
  e <- Log.io $ doesFileExist $ pFile "."
  let f = if e then "." else projectName p
  Log.io $ Y.encodeFile (pFile f) p
