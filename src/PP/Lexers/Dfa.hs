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
    ) where

import qualified Data.Graph.Inductive.Graph as Gr
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
  } deriving (Show)

-- |Lexer instance for DFA configuration
instance Lexer DfaConfig where
  simulate = simulateDfa
  consumed c = head (dfaInput c) == IToken0
  output = reverse . dfaOutput
  consume c = if consumed c then simulate c else consume $ simulate c

-- |Create DFA configuration
dfaConfig :: String -> DfaGraph -> DfaConfig
dfaConfig s g = DfaConfig (iToken s) [] [] g [findInitial g]

-- |Create a complete DFA from a list of lexical rules
createDfa :: [Rule] -> DfaGraph
createDfa = buildDfa . combineNfa . map createNfa . regexfy
  where
    createNfa (Rule n (RegEx re:_)) =
      case parseAst re :: To RegExpr of
        Left e    -> error $ show e
        Right ast -> buildNfa' n ast

-- |Simulate one iteration
simulateDfa :: DfaConfig -> DfaConfig
simulateDfa c@(DfaConfig (i:is) bs os g ps@(p:_)) =
  case findNext g i p of
    Nothing -> reducePath c
    Just q  -> DfaConfig is (i:bs) os g (q:ps)

-- |Find next node
findNext :: DfaGraph -> IToken -> Gr.LNode DfaNode -> Maybe (Gr.LNode DfaNode)
findNext _ IToken0 _ = Nothing
findNext g (IToken1 i) (n, _) =
  case map fst $ filter (\(_, DfaValue v) -> i == v) $ Gr.lsuc g n of
    []  -> Nothing
    [m] -> Just (m, fromMaybe DfaNode $ Gr.lab g m)

-- |Reduce path to initial node and construct an output token, if any
reducePath :: DfaConfig -> DfaConfig
reducePath c@(DfaConfig _ _ _ _ ((_, DfaInitial):_)) = c
reducePath (DfaConfig is (b:bs) os g ((_, DfaNode):ps)) =
  reducePath $ DfaConfig (b:is) bs os g ps
reducePath (DfaConfig is bs os g ((_, DfaFinal n):_)) =
  DfaConfig is [] (OToken2 n (reverse bs):os) g [findInitial g]

-- |Find initial node
findInitial :: DfaGraph -> Gr.LNode DfaNode
findInitial g = let [n] = filter isInitial (Gr.labNodes g) in n
  where
    isInitial (_, DfaInitial) = True
    isInitial _               = False
