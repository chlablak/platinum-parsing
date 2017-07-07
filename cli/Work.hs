{-|
Module      : Work
Description : Work system for PP ('.pp-work/' directory)
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module Work
    ( -- *Basics
      initialize
    , exists
      -- *Directory shortcuts
    , wDir
    , bDir
      -- *Binary files
    , save
    , load
    , loadOr
      -- *Files modification
    , register
    , modified
    , modified'
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Hash
import qualified Data.Yaml        as Y
import qualified Log
import           System.Directory
import           System.IO

-- |Work's directory
wDir :: FilePath -> FilePath
wDir p = ".pp-work/" ++ p
wFile = wDir "work.yaml"
bDir p = wDir "binaries/" ++ p

-- |Initialize the module if necessary
initialize :: Log.Logger
initialize = do
  Log.pushTag "work"
  cwd <- Log.io getCurrentDirectory
  Log.info $ "current working directory: " ++ cwd
  e <- exists $ wDir ""
  unless e $ do
    Log.info $ "create directory: " ++ wDir ""
    Log.io $ createDirectory $ wDir ""
    Log.io $ writeFile wFile ""
  Log.popTag

-- |Save a binary file
save :: (Binary a) => FilePath -> a -> Log.Logger
save p v = do
  Log.pushTag "work"
  Log.info $ "save binary file: " ++ bDir p
  Log.io $ encodeFile (bDir p) v
  Log.popTag

-- |Load a binary file
load :: (Binary a) => FilePath -> Log.LoggerIO a
load p = do
  Log.pushTag "work"
  Log.info $ "load binary file: " ++ bDir p
  Log.popTag
  Log.io $ decodeFile $ bDir p

-- |Check if a file exists
exists :: FilePath -> Log.LoggerIO Bool
exists p = do
  Log.pushTag "work"
  e <- Log.io $ doesPathExist p
  if e then
    Log.info $ "file does exist: " ++ p
  else
    Log.info $ "file doesn't exist: " ++ p
  Log.popTag
  return e

-- |Load a binary file if any, otherwise uses computation
loadOr :: (Binary a) => FilePath -> Log.LoggerIO a -> Log.LoggerIO a
loadOr p c = do
  e <- exists $ bDir p
  if e then
    load $ bDir p
  else do
    Log.pushTag "work"
    Log.info "use computation..."
    Log.popTag
    c

-- |Register a file state
register :: FilePath -> Log.Logger
register = undefined

-- |Check if a file has changed
modified :: FilePath -> Log.LoggerIO Bool
modified = undefined

-- |Check if a file has changed, if true: register it again
modified' :: FilePath -> Log.LoggerIO Bool
modified' = undefined
