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
wDir2 a b = wDir a ++ "/" ++ b
wFile = wDir "work.yaml"

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
reuse w p k c = do
  Log.pushTag "work"
  r <-  if w then do
          m <- modified p k
          if m then do
            Log.info $ show (p, k) ++ " modified, use computation and save"
            computeAndSave p k c
          else do
            Log.info $ show (p, k) ++ " not modified"
            e <- exists' p k
            if e then do
              Log.info "use previous work"
              load p k
            else do
              Log.info "no previous work, use computation and save"
              computeAndSave p k c
        else do
          Log.info "disabled, use computation"
          c
  Log.popTag
  return r
  where
    computeAndSave p k c = do
      v <- c
      save p k v
      register p k
      return v

-- |Save a binary file
save :: (Binary a) => FilePath -> String -> a -> Log.Logger
save p k v = do
  (n, _) <- getHash p
  Log.io $ createDirectoryIfMissing False (wDir n)
  Log.io $ encodeFile (wDir2 n k) v

-- |Load a binary file
load :: (Binary a) => FilePath -> String -> Log.LoggerIO a
load p k = do
  (n, _) <- getHash p
  Log.io $ decodeFile $ wDir2 n k

-- |Check if a file exists
exists :: FilePath -> Log.LoggerIO Bool
exists p = Log.io $ doesPathExist p
exists' p k = do
  (n, _) <- getHash p
  exists $ wDir2 n k

-- |Register a file state
register :: FilePath -> String -> Log.Logger
register p k = do
  (n, v) <- getHash p
  c <- getConfig
  setConfig $ case filter (\f -> fileid f == n) (files c) of
    [] -> Config $ ConfigFile p n [ConfigHash k v] : files c
    [f] -> Config $ ConfigFile p n
      (ConfigHash k v : filter (\h -> hashname h /= k) (filehash f))
        : filter (\f -> fileid f /= n) (files c)

-- |Check if a file has changed
modified :: FilePath -> String -> Log.LoggerIO Bool
modified p k = do
  (n, v) <- getHash p
  c <- getConfig
  case filter (\f -> fileid f == n) (files c) of
    [] -> return True
    [f] -> case filter (\h -> hashname h == k) (filehash f) of
      []  -> return True
      [h] -> return $ hashvalue h /= v

-- |Get the hash of a file
getHash :: FilePath -> Log.LoggerIO (String, String)
getHash p = do
  f <- Log.io $ readFile p
  return (hash' p, hash' f)
  where
    hash' = show . asWord64 . hash

-- |Working file structure
newtype Config = Config
  { files :: [ConfigFile]
  } deriving (Eq, Show)
data ConfigFile = ConfigFile
  { filepath :: FilePath
  , fileid   :: String
  , filehash :: [ConfigHash]
  } deriving (Eq, Show)
data ConfigHash = ConfigHash
  { hashname  :: String
  , hashvalue :: String
  } deriving (Eq, Show)

instance Y.FromJSON Config where
  parseJSON (Y.Object v) = Config <$> v .: "files"
  parseJSON _            = return $ Config []
instance Y.ToJSON Config where
  toJSON v = Y.object ["files" .= files v]

instance Y.FromJSON ConfigFile where
  parseJSON (Y.Object v) = ConfigFile <$> v .: "path" <*> v .: "id" <*> v .: "hash"
instance Y.ToJSON ConfigFile where
  toJSON v = Y.object ["path" .= filepath v, "id" .= fileid v, "hash" .= filehash v]

instance Y.FromJSON ConfigHash where
  parseJSON (Y.Object v) = ConfigHash <$> v .: "name" <*> v .: "value"
instance Y.ToJSON ConfigHash where
  toJSON v = Y.object ["name" .= hashname v, "value" .= hashvalue v]

-- |Get the config
getConfig :: Log.LoggerIO Config
getConfig = do
  c <- Log.io $ Y.decodeFile wFile
  return $ fromMaybe (Config []) c

-- |Set the config
setConfig :: Config -> Log.Logger
setConfig c = Log.io $ Y.encodeFile wFile c
