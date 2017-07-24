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
import           Control.Monad       (unless, when)
import           Data.Semigroup      ((<>))
import qualified Log
import           Options.Applicative
import qualified Project
import qualified Work

-- |Command arguments
commandArgs :: Parser CommandArgs
commandArgs = BuildCmd <$> buildArgs
  where
    buildArgs = BuildArgs
      <$> switch ( long "no-template"
        <> help "Disable templates compilation" )
      <*> switch ( long "no-test"
        <> help "Disable tests execution" )
      <*> strOption ( long "test-with"
        <> short 't'
        <> metavar "FILENAME"
        <> value ""
        <> help "Test the grammar on a source file" )
      <*> switch ( long "ast"
        <> help "Print the parsed AST (with --test-with)" )
      <*> strOption ( long "ast-to-html"
        <> metavar "FILENAME"
        <> value ""
        <> help "Output the parsed AST to HTML list" )

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

      when (useWork cargs)
        Work.initialize

      -- EBNF checks
      Log.info "EBNF checks:"
      let ebnf = EbnfArgs file False False False True False False False
      Cmd.Ebnf.dispatch $ Args cargs $ EbnfCmd ebnf

      checkOk <- Log.ok
      when checkOk $ do

        -- LALR generation
        Log.info "LALR generation:"
        let lalr = LalrArgs file False (-1) False (buildTestWith args) "" False (buildShowAst args) (buildAstHtml args) False
        Cmd.Lalr.dispatch $ Args cargs $ LalrCmd lalr

        genOk <- Log.ok
        when genOk $ do

          -- Templates compilation
          unless (disableTemplate args) $ do
            Log.info "Templates compilation:"
            Log.autoFlush False
            mapM_ (buildTemplate cargs lalr) $ Project.projectTemplates p
            Log.autoFlush True

          -- Tests
          unless (disableTest args) $ do
            Log.info "Tests execution:"
            Log.autoFlush False
            mapM_ (buildTest cargs lalr) $ Project.projectTests p
            Log.autoFlush True

  -- End
  Log.popTag
  return ()

-- |Compute the correct common args
mergeCArgs :: CommonArgs -> Project.Project -> CommonArgs
mergeCArgs (CommonArgs l s _ p) pr =
  CommonArgs l s (Project.projectUseWork pr) p

-- |Build template
buildTemplate :: CommonArgs -> LalrArgs -> Project.ProjectTemplate -> Log.Logger
buildTemplate cargs (LalrArgs l1 l2 l3 l4 l5 _ l7 l8 l9 l10) t = do
  Log.pushTag "template"
  Log.info $ Project.templateFile t ++ " > " ++ Project.templateDst t
  Log.flushAll
  let args = LalrArgs l1 l2 l3 l4 l5 (Project.templateFile t) l7 l8 l9 l10
  Cmd.Lalr.dispatch $ Args cargs $ LalrCmd args
  Log.flushOutToFile $ Project.templateDst t
  Log.popTag

-- |Build test
buildTest :: CommonArgs -> LalrArgs -> Project.ProjectTest -> Log.Logger
buildTest cargs (LalrArgs l1 l2 l3 l4 _ l6 l7 _ l9 l10) t = do
  Log.pushTag "test"
  Log.info $ Project.testFile t ++ if Project.testAstDst t /= ""
                                   then " > " ++ Project.testAstDst t
                                   else ""
  Log.flushAll
  let args = LalrArgs l1 l2 l3 l4 (Project.testFile t) l6 l7 (Project.testAstDst t /= "") l9 l10
  Cmd.Lalr.dispatch $ Args cargs $ LalrCmd args
  if Project.testAstDst t /= "" then
    Log.flushOutToFile $ Project.testAstDst t
  else
    Log.flushOutOnly
  Log.popTag
