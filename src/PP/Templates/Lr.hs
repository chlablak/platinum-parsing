{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : PP.Templates.Lr
Description : LR template
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}
module PP.Templates.Lr
    ( LrContext(..)
    , LrContextState(..)
    , LrContextTerm(..)
    , LrContextNonTerm(..)
    , LrContextTable(..)
    , LrContextTableRow(..)
    , LrContextAction(..)
    , context
    ) where

import           Data.Char
import           Data.Data
import qualified Data.List                           as L
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Typeable
import           PP.Builder
import           PP.Rule
import           PP.Template
import           Text.StringTemplate
import           Text.StringTemplate.GenericStandard

-- |LR table context
data LrContext = LrContext
  { states   :: [LrContextState]    -- ^States informations
  , terms    :: [LrContextTerm]     -- ^Terminals informations
  , nonTerms :: [LrContextNonTerm]  -- ^Non-terminals informations (without length)
  , table    :: LrContextTable      -- ^LR table informations
  } deriving (Data, Typeable, Eq)
data LrContextState = LrContextState
  { id  :: Int                      -- ^State ID
  , alt :: LrContextNonTerm         -- ^Associated non-terminal (not impl. yet)
  } deriving (Data, Typeable, Eq)
data LrContextTerm = LrContextTerm
  { symbol  :: Char                 -- ^Terminal symbol
  , isEmpty :: Bool                 -- ^It's the EMPTY symbol?
  } deriving (Data, Typeable, Eq)
data LrContextNonTerm = LrContextNonTerm
  { name   :: String                -- ^Non-terminal name
  , length :: Int                   -- ^Rule length (right side)
  } deriving (Data, Typeable, Eq)
data LrContextTable = LrContextTable
  { rows  :: [LrContextTableRow]    -- ^Table flatten in rows only
  , total :: Int                    -- ^Total rows (with errors)
  } deriving (Data, Typeable, Eq)
data LrContextTableRow = LrContextTableRow
  { state   :: LrContextState       -- ^Row state
  , isTerm  :: Bool                 -- ^Row state is associated with ?
  , term    :: LrContextTerm        -- ^Row is associated with terminal
  , nonTerm :: LrContextNonTerm     -- ^Row is associated with non-terminal
  , action  :: LrContextAction      -- ^Associated action
  } deriving (Data, Typeable, Eq)
data LrContextAction = LrContextAction
  { isReduce :: Bool                -- ^Is action reduce?
  , isShift  :: Bool                -- ^Is action shift?
  , isGoto   :: Bool                -- ^Is action goto?
  , isError  :: Bool                -- ^Is action error?
  , isAccept :: Bool                -- ^Is action accept?
  , shift    :: Int                 -- ^Shift value
  , goto     :: Int                 -- ^Goto value
  , reduce   :: LrContextNonTerm    -- ^Reduce associated non-terminal (with length)
  } deriving (Data, Typeable, Eq)

-- |Construct the LR context
context :: LrTable -> LrContext
context t = LrContext states' terms' nonTerms' table'
  where
    states' = L.nub [LrContextState i (nonTerm' Empty) | ((i, _), _) <- list']
    terms' = term' Empty : L.nub [term' r | ((_, r), _) <- list', isTerm' r]
    nonTerms' = L.nub [nonTerm' r | ((_, r), _) <- list', isNonTerm' r]
    table' = LrContextTable rows'
      (L.length states' * (L.length terms' + L.length nonTerms'))
    rows' = [LrContextTableRow (LrContextState i (nonTerm' Empty))
                               (isTermOrEmpty' r)
                               (term' r)
                               (nonTerm' r)
                               (action' a)
           | ((i, r), a) <- list']
    term' (Term x) = LrContextTerm x False
    term' Empty    = LrContextTerm (chr 0) True
    term' _        = LrContextTerm (chr 0) False
    nonTerm' (NonTerm n) = LrContextNonTerm n (-1)
    nonTerm' _           = LrContextNonTerm "" (-1)
    action' (LrReduce (Rule n xs)) =
      LrContextAction True False False False False (-1) (-1)
        (LrContextNonTerm n (L.length xs - 1))
    action' (LrShift s) =
      LrContextAction False True False False False s (-1) (nonTerm' Empty)
    action' (LrGoto s) =
      LrContextAction False False True False False (-1) s (nonTerm' Empty)
    action' LrError =
      LrContextAction False False False True False (-1) (-1) (nonTerm' Empty)
    action' LrAccept =
      LrContextAction False False False False True (-1) (-1) (nonTerm' Empty)
    list' = Map.toList t
    isTerm' (Term _) = True
    isTerm' _        = False
    isTermOrEmpty' (Term _) = True
    isTermOrEmpty' Empty    = True
    isTermOrEmpty' _        = False
    isNonTerm' (NonTerm _) = True
    isNonTerm' _           = False
    isReduce' (LrReduce _) = True
    isReduce' _            = False

-- |Template instance for LrContext
instance Template LrContext where
  attributes = setAttribute "lr"
