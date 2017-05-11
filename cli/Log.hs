{-|
Module      : Log
Description : Logging system for the CLI
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Log
    ( msg
    , task
    ) where

import           Control.Monad
import qualified Data.List     as L

msg :: Bool -> Int -> String -> String -> IO ()
msg verbose indent tag str = when verbose $ do
  when (indent > 0) $ putStr $ L.replicate indent ' '
  when (tag /= "") $ putStr $ "[" ++ tag ++ "] "
  putStrLn str

task :: Bool -> String -> (() -> a) -> IO a
task verbose str func = do
  msg verbose 0 "TASK" str
  r <- exec
  msg verbose 2 "OK" str
  return r
  where
    exec = let a = func () in seq a (return a)
