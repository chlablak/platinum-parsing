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
    ( Logger
    -- ** High-API
    , start
    --, setLevel
    , flushAll
    , pushTag
    , popTag
    , pushTask
    , popTask
    , autoFlush
    -- ** Low-API
    , pushMsg
    , pushOut
    -- **Shortcuts
    , io
    --, off
    , fatal
    , err
    , warn
    , info
    , debug
    , trace
    , abort
    , out
    , getLog
    , task
    ) where

import           Control.Monad.State
import           Data.Char
import           System.Clock

-- |Alias
type Tag = String
type Level = Int
type Task = String

-- |Logging messages
data Message
  = PushMsg Level String  -- ^Push a new message with a specific level
  | PushOut String        -- ^Push a new message to output
  | PushTag Tag           -- ^Push a new tag
  | PopTag                -- ^Pop the top tag
  | SetLevel Level        -- ^Set veborsity level
  | AutoFlush Bool        -- ^Set auto flush on/off
  | None                  -- ^Nothing to do
    deriving Show

-- |Logger
type Log = (Bool, Level, [Tag], [(Task, TimeSpec)], [Message])
type Logger = StateT Log IO ()

-- |IO operation
io :: IO a -> StateT Log IO a
io = liftIO

-- |Pre-defined shortcuts
off = setLevel 1000
fatal m = do { pushTag "fatal"; pushMsg 60 m; popTag }
err m = do { pushTag "error"; pushMsg 50 m; popTag }
warn m = do { pushTag "warning"; pushMsg 40 m; popTag }
info m = do { pushTag "info"; pushMsg 30 m; popTag }
debug m = do { pushTag "debug"; pushMsg 20 m; popTag }
trace m = do { pushTag "trace"; pushMsg 10 m; popTag }
abort = fatal "aborting..."
out = pushOut
getLog = (False, 0 :: Int, [], [], [])
task x = x `seq` x

-- |Start a new logger
start :: Level -> Tag -> Logger
start l t = put (False, l, [t], [], [])

-- |Push a new tag
pushTag :: Tag -> Logger
pushTag t = do
  (af, l, ts, ks, ms) <- get
  put (af, l, ts, ks, PushTag t : ms)

-- |Pop the top tag
popTag :: Logger
popTag = do
  (af, l, ts, ks, ms) <- get
  put (af, l, ts, ks, PopTag : ms)

-- |Push a new message with a specific level
pushMsg :: Level -> String -> Logger
pushMsg l m = do
  (af, l2, ts, ks, ms) <- get
  put (af, l2, ts, ks, PushMsg l m : ms)
  when af flushAll

-- |Push a new message to be outputed
pushOut :: String -> Logger
pushOut m = do
  (af, l, ts, ks, ms) <- get
  put (af, l, ts, ks, PushOut m : ms)
  when af flushAll

-- |Set verbosity level
setLevel :: Int -> Logger
setLevel l = do
  (af, l, ts, ks, ms) <- get
  put (af, l, ts, ks, SetLevel l : ms)

-- |Set auto flush on/off
autoFlush :: Bool -> Logger
autoFlush value = do
  (af, l, ts, ks, ms) <- get
  put (af, l, ts, ks, AutoFlush value : ms)
  when value flushAll

-- |Push a new task
pushTask :: Task -> Logger
pushTask k = do
  (af, l, ts, ks, ms) <- get
  start <- io $ getTime Monotonic
  put (af, l, ts, (k,start):ks, ms)
  pushTag "task"
  pushTag "start"
  pushMsg 30 k
  popTag
  popTag

-- |Pop the top task
popTask :: Logger
popTask = do
  (af, l, ts, (k,start):ks, ms) <- get
  put (af, l, ts, ks, ms)
  end <- io $ getTime Monotonic
  let TimeSpec { sec = _, nsec = nsec } = diffTimeSpec start end
  pushTag "task"
  pushTag "end"
  pushMsg 30 $ "in " ++ show (div nsec 1000000) ++ "ms, " ++ k
  popTag
  popTag

-- |Flush the logger to output
flushAll :: Logger
flushAll = do
  (af, l, ts, ks, ms) <- get
  flushAll' (af, l, ts, ks, reverse ms)
  where
    flushAll' l@(_, _, _, _, []) = put l
    flushAll' (af, l, ts, ks, PushMsg l2 m:ms) = do
      when (l <= l2) $ do
        io $ putTag ts
        io $ putStrLn m
      flushAll' (af, l, ts, ks, ms)
    flushAll' (af, l, ts, ks, PushOut m:ms) = do
      io $ putStrLn m
      flushAll' (af, l, ts, ks, ms)
    flushAll' (af, l, ts, ks, PushTag t:ms) = flushAll' (af, l, t:ts, ks, ms)
    flushAll' (af, l, _:ts, ks, PopTag:ms) = flushAll' (af, l, ts, ks, ms)
    flushAll' (af, _, ts, ks, SetLevel l:ms) = flushAll' (af, l, ts, ks, ms)
    flushAll' (_, l, ts, ks, AutoFlush af:ms) = flushAll' (af, l, ts, ks, ms)
    flushAll' (af, l, ts, ks, None:ms) = flushAll' (af, l, ts, ks, ms)
    putTag ts =
      putStr $ concatMap (\t -> "[" ++ map toUpper t ++ "]") (reverse ts) ++ " "
