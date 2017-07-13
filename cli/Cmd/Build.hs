{-|
Module      : Cmd.Build
Description : CLI for the `pp build` command
Copyright   : (c) 2017 Patrick Champion
License     : see LICENSE file
Maintainer  : chlablak@gmail.com
Stability   : provisional
Portability : portable
-}

module Cmd.Build
    ( commandArgs
    , dispatch
    ) where

import           Args
import qualified Cmd.Ebnf
import qualified Cmd.Lalr
import           Control.Monad       (when)
import           Data.Semigroup      ((<>))
import qualified Log
import           Options.Applicative
import qualified Project

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = BuildCmd <$> buildArgs
  where
    buildArgs = BuildArgs
      <$> switch ( long "no-template"
        <> help "Disable templates compilation" )

-- |Command dispatch
dispatch :: Args -> Log.Logger
dispatch (Args cargs0 (BuildCmd args)) = do
  Log.pushTag "build"

  -- Parse pp.yaml
  p <- Project.get
  case p of
    Project.NoProject -> Log.err "no project in current directory"
    Project.MalformedProject err -> Log.err $ "malformed project: " ++ err
    _ -> do
      Log.info $ "build project: " ++ Project.projectName p
      let cargs = mergeCArgs cargs0 p
      let file = head $ Project.projectGrammars p

      -- EBNF checks
      Log.info "EBNF checks"
      let ebnf = EbnfArgs file False False False True False False
      Cmd.Ebnf.dispatch $ Args cargs $ EbnfCmd ebnf

      checkOk <- Log.ok
      if checkOk then do

        -- LALR generation
        Log.info "LALR generation"
        let lalr = LalrArgs file False (-1) False "" "" False False
        Cmd.Lalr.dispatch $ Args cargs $ LalrCmd lalr

        genOk <- Log.ok
        if genOk && not (disableTemplate args) then do

          -- Templates compilation
          Log.info "Templates compilation"
          Log.autoFlush False
          mapM_ (buildTemplate cargs lalr) $ Project.projectTemplates p
          Log.autoFlush True

        else
          Log.none
      else
        Log.none

  -- End
  Log.popTag
  return ()

-- |Compute the correct common args
mergeCArgs :: CommonArgs -> Project.Project -> CommonArgs
mergeCArgs (CommonArgs l s _ p) pr =
  CommonArgs l s (Project.projectUseWork pr) p

-- |Build template
buildTemplate :: CommonArgs -> LalrArgs -> Project.ProjectTemplate -> Log.Logger
buildTemplate cargs (LalrArgs l1 l2 l3 l4 l5 _ l7 l8) t = do
  Log.pushTag "template"
  Log.info $ Project.templateFile t ++ " > " ++ Project.templateDst t
  Log.flushAll
  let args = LalrArgs l1 l2 l3 l4 l5 (Project.templateFile t) l7 l8
  Cmd.Lalr.dispatch $ Args cargs $ LalrCmd args
  Log.flushAllToFile $ Project.templateDst t
  Log.popTag
