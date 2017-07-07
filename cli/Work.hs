{-|
Module      : Work
Description : Work system for PP ('.pp-work/' directory)
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Work
    ( initialize
    , reuse
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Hash
import           Data.Maybe
import qualified Data.Text        as T
import           Data.Yaml        ((.:), (.=))
import qualified Data.Yaml        as Y
import qualified Log
import           System.Directory
import           System.IO

-- |Work's directory
wDir :: FilePath -> FilePath
wDir p = ".pp-work/" ++ p
wFile = wDir "work.yaml"
bDir p = wDir "binaries/" ++ p

-- |Initialize module if necessary
initialize :: Log.Logger
initialize = do
  cwd <- Log.io getCurrentDirectory
  Log.info $ "current working directory: " ++ cwd
  e <- exists $ wDir ""
  unless e $ do
    Log.info $ "create directory: " ++ wDir ""
    Log.io $ createDirectory $ wDir ""
    Log.io $ writeFile wFile ""

-- |Reuse previous computation, if any
reuse :: (Binary a) => Bool -> FilePath -> String -> Log.LoggerIO a -> Log.LoggerIO a
reuse w p k c = undefined

-- |Save a binary file
save :: (Binary a) => FilePath -> a -> Log.Logger
save p v = Log.io $ encodeFile p v

-- |Load a binary file
load :: (Binary a) => FilePath -> Log.LoggerIO a
load p = Log.io $ decodeFile p

-- |Check if a file exists
exists :: FilePath -> Log.LoggerIO Bool
exists p = Log.io $ doesPathExist p

-- |Register a file state
register :: FilePath -> Log.Logger
register p = do
  Config cf <- getConfig
  h <- getHash p
  let c = Config $ ConfigFile p h : filter (\f -> filepath f /= p) cf
  setConfig c

-- |Check if a file has changed
modified :: FilePath -> Log.LoggerIO Bool
modified p = do
  Config cf <- getConfig
  case filter (\f -> filepath f == p) cf of
    []               -> return True
    [ConfigFile _ h] -> do
      h2 <- getHash p
      return $ h /= h2

-- |Get the hash of a file
getHash :: FilePath -> Log.LoggerIO T.Text
getHash p = do
  f <- Log.io $ readFile p
  return $ T.pack $ show $ asWord64 $ hash f

-- |Working file structure
newtype Config = Config
  { files :: [ConfigFile]
  } deriving (Eq, Show)
data ConfigFile = ConfigFile
  { filepath :: FilePath
  , filehash :: T.Text
  } deriving (Eq, Show)

instance Y.FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "files"
  parseJSON _            = return $ Config []
instance Y.ToJSON Config where
  toJSON v = Y.object ["files" .= files v]

instance Y.FromJSON ConfigFile where
  parseJSON (Y.Object v) = ConfigFile <$> v .: "path" <*> v .: "hash"
instance Y.ToJSON ConfigFile where
  toJSON v = Y.object ["path" .= filepath v, "hash" .= filehash v]

-- |Get the config
getConfig :: Log.LoggerIO Config
getConfig = do
  c <- Log.io $ Y.decodeFile wFile
  return $ fromMaybe (Config []) c

-- |Set the config
setConfig :: Config -> Log.Logger
setConfig c = Log.io $ Y.encodeFile wFile c
