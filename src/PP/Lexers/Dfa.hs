{-|
Module      : PP.Lexers.Dfa
Description : Lexer simulation with DFA
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Lexers.Dfa
    ( DfaConfig
    , dfaConfig
    , createDfa
    , createDfa'
    ) where

import           Control.Exception
import           Data.Either
import qualified Data.Graph.Inductive.Graph as Gr
import qualified Data.List                  as L
import           Data.Maybe
import           PP.Builder
import           PP.Builders.Dfa
import           PP.Builders.Nfa
import           PP.Grammar
import           PP.Grammars.Lexical
import           PP.Lexer
import           PP.Rule

-- |DFA configuration
data DfaConfig = DfaConfig
  { dfaInput  :: [IToken]             -- ^Input tokens
  , dfaBuffer :: [IToken]             -- ^Buffer
  , dfaOutput :: [OToken]             -- ^Output tokens
  , dfaGraph  :: DfaGraph             -- ^Automaton
  , dfaPath   :: [Gr.LNode DfaNode]   -- ^Path for the current buffer
  }

instance Show DfaConfig where
  show (DfaConfig is bs os _ ps) =
    "DfaConfig {dfaInput = " ++ show is ++
    ", dfaBuffer = " ++ show bs ++
    ", dfaOutput = " ++ show os ++
    ", dfaGraph = ..., dfaPath = " ++ show ps ++ "}"

-- |Lexer instance for DFA configuration
-- Dragon Book (2nd edition, fr), page 156, example 3.28
instance Lexer DfaConfig where
  simulate = simulateDfa
  consumed c = null $ dfaInput c
  output = reverse . dfaOutput
  consume c = if consumed c then simulate c else consume $ simulate c

-- |Create DFA configuration
dfaConfig :: String -> DfaGraph -> DfaConfig
dfaConfig s g = DfaConfig s [] [] g [findInitial g]

-- |Create a complete DFA from a list of lexical rules
-- Rules are sorted normally, except that underscore '_' is less than 'A'
createDfa :: [Rule] -> DfaGraph
createDfa = buildDfa . combineNfa . map createNfa . sort . regexfy
  where
    createNfa (Rule n (RegEx re:_)) =
      case parseAst re :: To RegExpr of
        Left e    -> error $ show e
        Right ast -> buildNfa' n ast
    sort = L.sortBy (\(Rule a _) (Rule b _) -> sort' a b)
    sort' ('_':a) ('_':b) = sort' a b
    sort' ('_':_) _       = LT
    sort' _ ('_':_)       = GT
    sort' a b             = compare a b

-- |Exception-safe version of `createDfa`
createDfa' :: [Rule] -> IO (Either String DfaGraph)
createDfa' rs = do
    a <- try (evaluate $ createDfa rs) :: IO (Either SomeException DfaGraph)
    case a of
        Left e  -> return $ Left $ head $ lines $ displayException e
        Right r -> return $ Right r

-- |Simulate one iteration
simulateDfa :: DfaConfig -> DfaConfig
simulateDfa c@(DfaConfig [] _ _ _ _) = reducePath c
simulateDfa c@(DfaConfig (i:is) bs os g ps@(p:_)) =
  case findNext g i p of
    Nothing -> reducePath c
    Just q  -> DfaConfig is (i:bs) os g (q:ps)

-- |Find next node
findNext :: DfaGraph -> IToken -> Gr.LNode DfaNode -> Maybe (Gr.LNode DfaNode)
findNext g i (n, _) =
  case map fst $ filter (\(_, DfaValue v) -> i == v) $ Gr.lsuc g n of
    []  -> Nothing
    [m] -> Just (m, fromMaybe DfaNode $ Gr.lab g m)

-- |Reduce path to initial node and construct an output token, if any
reducePath :: DfaConfig -> DfaConfig
reducePath c@(DfaConfig [] _ _ _ ((_, DfaInitial):_)) = c
reducePath (DfaConfig (_:is) bs os g ps@((_, DfaInitial):_)) =
  DfaConfig is bs os g ps
reducePath (DfaConfig is (b:bs) os g ((_, DfaNode):ps)) =
  reducePath $ DfaConfig (b:is) bs os g ps
reducePath (DfaConfig is bs os g ((_, DfaFinal n):_)) =
  DfaConfig is [] (OToken2 (reverse bs) n:os) g [findInitial g]

-- |Find initial node
findInitial :: DfaGraph -> Gr.LNode DfaNode
findInitial g = let [n] = filter isInitial (Gr.labNodes g) in n
  where
    isInitial (_, DfaInitial) = True
    isInitial _               = False
