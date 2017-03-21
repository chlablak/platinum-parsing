module Main where

import Lib
import qualified EBNF as E
import qualified Text.Parsec as P
import System.Directory
import Data.Either
import System.Clock
import Control.Monad

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  print cwd

  grammar <- readFile "../grammar.ebnf"
  let g = concat (lines grammar)
  print g

  print $ P.parse E.statements "" g

  let (Right s) = P.parse E.statements "" g
  printAST s

  start <- getTime Monotonic
  forM_ [1..1000000] (\_ -> return (P.parse E.statements "" g))
  end <- getTime Monotonic
  let TimeSpec {sec = _, nsec = nsec} = diffTimeSpec start end
  putStrLn $ show (nsec `div` 1000000) ++ "ms"

printAST :: E.TStatement -> IO ()
printAST (E.TStatements []) = return ()
printAST (E.TStatements (x:xs)) = do
  printAST x
  printAST (E.TStatements xs)
printAST (E.TDefinition r e) = putStrLn $ "def: " ++ show r ++ " -> " ++ show e
